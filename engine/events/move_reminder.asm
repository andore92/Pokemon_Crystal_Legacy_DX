;---------------------------------------------------------------------
; event/move_reminder.asm
;---------------------------------------------------------------------
; Include our battle structure constants and the new move definitions.
INCLUDE "constants/battle_constants.asm"
INCLUDE "constants/move_constants.asm"

; Based on an old commit of Rangi's Polished Crystal, which was in turn based off TPP Anniversary Crystal
; https://github.com/Rangi42/polishedcrystal/blob/39bf603531d74254e7ab2740677d38ec3ef9b6bd/event/move_reminder.asm
; https://github.com/TwitchPlaysPokemon/tppcrystal251pub/blob/public/event/move_relearner.asm

EggMoveReminder::
    ld hl, Text_EggMoveReminderIntro
    call RemindStart
    ret c
    call GetEggRemindableMoves
    jp ReminderGotMove

MoveReminder::
    ld hl, Text_MoveReminderIntro
    call RemindStart
    ret c
    call GetRemindableMoves
    jp ReminderGotMove

RemindStart:
    call PrintText
    call YesNoBox
    jp c, .cancel

    ld hl, Text_MoveReminderWhichMon
    call PrintText
    call JoyWaitAorB

    farcall SelectMonFromParty
    jr c, .cancel

    ld a, [wCurPartySpecies]
    cp EGG
    jr z, .egg

    call IsAPokemon
    jr c, .no_mon
    ret
.cancel:
    ld hl, Text_MoveReminderCancel
    jr .done
.egg:
    ld hl, Text_MoveReminderEgg
    jr .done
.no_mon:
    ld hl, Text_MoveReminderNoMon
.done:  
    call PrintText
    scf
    ret

ReminderGotMove:
    jr z, .no_moves

    ld hl, Text_MoveReminderWhichMove
    call PrintText
    call JoyWaitAorB

    call ChooseMoveToLearn
    jr c, .skip_learn

    ld a, [wMenuSelection]
    ld [wTempByteValue], a   ; alternatively: ld [wd265], a
    call GetMoveName
    ld hl, wStringBuffer1
    ld de, wStringBuffer2
    ld bc, wStringBuffer2 - wStringBuffer1
    call CopyBytes
    ld b, 0
    predef LearnMove
    ld a, b
    and a
    jr z, .skip_learn
    ; fallthrough:
.skip_learn:
    call ReturnToMapWithSpeechTextbox
    ld hl, Text_MoveReminderCancel
    jp PrintText

.no_moves:
    ld hl, Text_MoveReminderNoMoves
    jp PrintText


GetRemindableMoves::
; Get moves remindable by CurPartyMon
; Returns z if no moves can be reminded.
    ld hl, wd002
    xor a
    ld [hli], a
    ld [hl], $ff

    ld a, MON_SPECIES
    call GetPartyParamLocation
    ld a, [hl]
    ld [wCurPartySpecies], a

    push af
    ld a, MON_LEVEL
    call GetPartyParamLocation
    ld a, [hl]
    ld [wCurPartyLevel], a

    ld b, 0
    ld de, wd002 + 1

; Based on GetEggMove in engine/pokemon/breeding/egg.asm
    ld a, [wCurPartySpecies]
    dec a
    push bc
    ld c, a
    ld hl, EvosAttacksPointers
    add hl, bc
    add hl, bc
    ld a, BANK(EvosAttacksPointers)
    call GetFarWord
.skip_evos:
    ld a, BANK("Evolutions and Attacks")
    call GetFarByte
    inc hl
    and a
    jr nz, .skip_evos

.loop_moves:
    ld a, BANK("Evolutions and Attacks")
    call GetFarByte
    inc hl
    and a
    jr z, .done
    ld c, a
    ld a, [wCurPartyLevel]
    cp c
    ld a, BANK("Evolutions and Attacks")
    call GetFarByte
    inc hl
    jr c, .loop_moves

    ld c, a
    call CheckAlreadyInList
    jr c, .loop_moves
    call CheckPokemonAlreadyKnowsMove
    jr c, .loop_moves
    ld a, c
    ld [de], a
    inc de
    ld a, $ff
    ld [de], a
    pop bc
    inc b
    push bc
    jr .loop_moves

.done:
    pop bc
    pop af
    ld [wCurPartySpecies], a
    ld a, b
    ld [wd002], a
    and a
    ret

GetEggRemindableMoves:
; Get moves remindable by CurPartyMon
; Returns z if no moves can be reminded.
    ld hl, wd002
    xor a
    ld [hli], a
    ld [hl], $ff

    ld a, MON_SPECIES
    call GetPartyParamLocation
    ld a, [hl]
    ld [wCurPartySpecies], a

    push af
    ld a, MON_LEVEL
    call GetPartyParamLocation
    ld a, [hl]
    ld [wCurPartyLevel], a

    ld b, 0
    ld de, wd002 + 1

; Based on GetEggMove in engine/pokemon/breeding/egg.asm
    ld a, [wCurPartySpecies]
    dec a
    push bc
    ld c, a
    ld hl, EggMovePointers      ; EggMovePointers replaces EvosAttacksPointers
    add hl, bc
    add hl, bc
    ld a, BANK(EggMovePointers)
    call GetFarWord

.loop_moves:
    ld a, BANK("Egg Moves")
    call GetFarByte
    inc hl
    cp -1    ; last entry in egg move table is -1
    jr z, .done
    ld c, a

    call CheckAlreadyInList
    jr c, .loop_moves
    call CheckPokemonAlreadyKnowsMove
    jr c, .loop_moves
    ld a, c
    ld [de], a
    inc de
    ld a, $ff
    ld [de], a
    pop bc
    inc b
    push bc
    jr .loop_moves

.done:
    pop bc
    pop af
    ld [wCurPartySpecies], a
    ld a, b
    ld [wd002], a
    and a
    ret

CheckAlreadyInList::
    push hl
    ld hl, wd002 + 1
.loop:
    ld a, [hli]
    inc a
    jr z, .nope
    dec a
    cp c
    jr nz, .loop
    pop hl
    scf
    ret
.nope:
    pop hl
    and a
    ret

CheckPokemonAlreadyKnowsMove::
    push hl
    push bc
    ld a, MON_MOVES
    call GetPartyParamLocation
    ld b, 4
.loop:
    ld a, [hli]
    cp c
    jr z, .yes
    dec b
    jr nz, .loop
    pop bc
    pop hl
    and a
    ret
.yes:
    pop bc
    pop hl
    scf
    ret

ChooseMoveToLearn::
; Number of items stored in wd002
; List of items stored in wd002 + 1
    call FadeToMenu
    farcall BlankScreen
    call UpdateSprites
    ld hl, .MenuHeader
    call CopyMenuHeader
    xor a
    ld [wMenuCursorPosition], a
    ld [wMenuScrollPosition], a
    call ScrollingMenu
    call SpeechTextbox
    ld a, [wMenuJoypad]
    cp B_BUTTON
    jr z, .carry
    ld a, [wMenuSelection]
    ld [wPutativeTMHMMove], a
    and a
    ret
.carry:
    scf
    ret

.MenuHeader:
    db MENU_BACKUP_TILES       ; flags
    menu_coords 1, 1, SCREEN_WIDTH - 1, 11
    dw .MenuData
    db 1                      ; default option

.MenuData:
    db SCROLLINGMENU_DISPLAY_ARROWS | SCROLLINGMENU_ENABLE_FUNCTION3  ; item format
    db 5, SCREEN_WIDTH + 2    ; rows, columns
    db SCROLLINGMENU_ITEMS_NORMAL
    ; dba  wd002
    dba  wd002
    dba .PrintMoveName
    dba .PrintDetails
    dba .PrintMoveDesc

.PrintMoveName:
    push de
    ld a, [wMenuSelection]
    ld [wTempByteValue], a   ; or: ld [wd265], a
    call GetMoveName
    pop hl
    jp PlaceString

;---------------------------------------------------------------------
; Revised .PrintDetails subroutine:
; This version extracts the combined move type/category field (at offset 3)
; and displays the move category (from .Classes) and the move type (from .Types).
; It then prints move power (offset 2) and PP (offset 5).
;---------------------------------------------------------------------
.PrintDetails:
    ; Clear the temporary string buffer (wStringBuffer1)
    ld hl, wStringBuffer1
    ld bc, wStringBuffer2 - wStringBuffer1
    ld a, ' '
    call ByteFill

    ; Check if selection is valid
    ld a, [wMenuSelection]
    inc a
    ret z
    dec a

    ; Compute pointer to the move entry:
    ld bc, MOVE_LENGTH       ; MOVE_LENGTH should be 7 in the new format
    ld hl, Moves
    call AddNTimes           ; HL now points to the selected move entry
    ; Save the pointer for later use in a temporary word storage.
    ld [wTempWord], hl

    ; --- Extract the combined move type/category field ---
    ld hl, [wTempWord]
    add hl, 3                ; Offset 3 holds the combined field
    ld a, [hl]              ; A = combined move type/category
    push af               ; Save combined value for later

    ; --- Extract and print move category ---
    ld hl, [wTempWord]
    add hl, 3
    ld a, [hl]
    and MOVE_CATEGORY_MASK   ; Isolate the upper nibble (move category bits)
    swap a                  ; Swap nibbles so that the category index is in the lower nibble
    ; Multiply the index by the string length; assume each category string is 4 bytes.
    add a                   ; a = 2 * category index
    add a                   ; a = 4 * category index
    ld b, 0
    ld c, a
    ld hl, .Classes         ; Pointer to move category strings (e.g., "Phys", "Spcl", "Stat")
    add hl, bc
    ld de, wStringBuffer1   ; Destination: start of the display string
    ld bc, 4                ; Copy 4 bytes (adjust if your entries are longer)
    call CopyBytes          ; Copy category abbreviation into the buffer
    ; Append a slash after the category
    ld hl, wStringBuffer1 + 4
    ld [hl], '/'

    ; --- Extract and print move type ---
    pop af                  ; Retrieve the combined field back (A now has the full combined byte)
    and MOVE_TYPE_MASK      ; Now A = move type index (lower nibble)
    ; Multiply type index by the string length; assume each type abbreviation is 4 bytes.
    add a
    add a                   ; a = 4 * move type index
    ld b, 0
    ld c, a
    ld hl, .Types           ; Pointer to move type strings (e.g., "NRM", "FGT", "FLY", etc.)
    add hl, bc
    ld de, wStringBuffer1 + 5 ; Place type string at offset 5 (after category and slash)
    ld bc, 4                ; Copy 4 bytes (adjust if needed)
    call CopyBytes
    ; Append a slash after the type abbreviation
    ld hl, wStringBuffer1 + 9
    ld [hl], '/'

    ; --- Print move power ---
    ld hl, [wTempWord]      ; Reload the move entry pointer
    add hl, 2               ; Move power is at offset 2
    ld a, [hl]             ; Get move power
    ld [EngineBuffer1], a
    ld hl, wStringBuffer1 + 10
    ld de, EngineBuffer1
    ld bc, 2                ; Print as a 2-digit number (adjust if necessary)
    call PrintNum

    ; --- Print move PP ---
    ld hl, [wTempWord]      ; Reload move entry pointer
    add hl, 5               ; PP is at offset 5
    ld a, [hl]
    ld [wTempByteValue], a
    ld hl, wStringBuffer1 + 12
    ld de, wTempByteValue
    ld bc, 2                ; Print PP in a 2-digit field
    call PrintNum

    ; Append a terminating symbol (e.g., "@")
    ld hl, wStringBuffer1 + 14
    ld [hl], '@'

    ; Output the constructed move detail string.
    ld hl, wStringBuffer1
    jp PlaceString
;---------------------------------------------------------------------
; End .PrintDetails subroutine
;---------------------------------------------------------------------

.Types:
    db "NRM@", "FGT@", "FLY@", "PSN@", "GRD@", "RCK@", "BRD@", "BUG@"
    db "GHT@", "STL@", "NRM@", "NRM@", "NRM@", "NRM@", "NRM@", "NRM@"
    db "NRM@", "NRM@", "NRM@", "???@", "FIR@", "WTR@", "GRS@", "ELC@"
    db "PSY@", "ICE@", "DRG@", "DRK@", "FRY@"

.Classes:
    db "Phys", "Spcl", "Stat"   ; Each entry is 4 bytes; adjust as needed.

.PrintMoveDesc:
    push de
    call SpeechTextbox
    ld a, [wMenuSelection]
    inc a
    pop de
    ret z
    dec a
    ld [wCurSpecies], a
    hlcoord 1, 14
    predef_jump PrintMoveDescription

Text_EggMoveReminderIntro:
    text_far _EggMoveReminderIntro
    text_end
Text_MoveReminderIntro:
    text_far _MoveReminderIntro
    text_end
Text_MoveReminderWhichMon:
    text_far _MoveReminderWhichMon
    text_end
Text_MoveReminderWhichMove:
    text_far _MoveReminderWhichMove
    text_end
Text_MoveReminderCancel:
    text_far _MoveReminderCancel
    text_end
Text_MoveReminderEgg:
    text_far _MoveReminderEgg
    text_end
Text_MoveReminderNoMon:
    text_far _MoveReminderNoMon
    text_end
Text_MoveReminderNoMoves:
    text_far _MoveReminderNoMoves
    text_end
