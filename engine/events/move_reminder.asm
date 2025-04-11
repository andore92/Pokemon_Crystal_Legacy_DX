;--------------------------------------------------------------
; Revised move_reminder.asm
; Based on an old commit of Rangi's Polished Crystal (which itself was
; based on TPP Anniversary Crystal) with modifications to display both
; the move category and move type from the merged move field.
; (Each move entry now is 7 bytes long, and the byte at offset 3 contains
; the lower nibble as the move type and the upper nibble as the move category.)
;--------------------------------------------------------------

MoveReminder:
    ld hl, Text_MoveReminderIntro
    call PrintText
    call JoyWaitAorB

    ld a, GOLD_LEAF
    ld [CurItem], a
    ld hl, NumItems
    call CheckItem
    jp nc, .no_gold_leaf

    ld hl, Text_MoveReminderPrompt
    call PrintText
    call YesNoBox
    jp c, .cancel

    ld hl, Text_MoveReminderWhichMon
    call PrintText
    call JoyWaitAorB

    ld b, $6
    callba SelectMonFromParty
    jr c, .cancel

    ld a, [CurPartySpecies]
    cp EGG
    jr z, .egg

    call IsAPokemon
    jr c, .no_mon

    call GetRemindableMoves
    jr z, .no_moves

    ld hl, Text_MoveReminderWhichMove
    call PrintText
    call JoyWaitAorB

    call ChooseMoveToLearn
    jr c, .skip_learn

    ld a, [MenuSelection]
    ld [wd265], a
    call GetMoveName
    ld hl, StringBuffer1
    ld de, StringBuffer2
    ld bc, StringBuffer2 - StringBuffer1
    call CopyBytes
    ld b, 0
    predef LearnMove
    ld a, b
    and a
    jr z, .skip_learn

    ld a, GOLD_LEAF
    ld [CurItem], a
    ld a, 1
    ld [wItemQuantityChangeBuffer], a
    ld a, -1
    ld [CurItemQuantity], a
    ld hl, NumItems
    call TossItem

    ld de, SFX_TRANSACTION
    call PlaySFX
    call WaitSFX

.skip_learn:
    call CloseSubmenu
    call SpeechTextBox
.cancel:
    ld hl, Text_MoveReminderCancel
    call PrintText
    ret

.egg:
    ld hl, Text_MoveReminderEgg
    call PrintText
    ret

.no_gold_leaf:
    ld hl, Text_MoveReminderNoGoldLeaf
    call PrintText
    ret

.no_mon:
    ld hl, Text_MoveReminderNoMon
    call PrintText
    ret

.no_moves:
    ld hl, Text_MoveReminderNoMoves
    call PrintText
    ret

;--------------------------------------------------------------
; GetRemindableMoves
; (This subroutine is unchanged from TPP’s version.)
;--------------------------------------------------------------
GetRemindableMoves:
    GLOBAL EvosAttacksPointers, EvosAttacks
    ld hl, wd002
    xor a
    ld [hli], a
    ld [hl], $FF

    ld a, MON_SPECIES
    call GetPartyParamLocation
    ld a, [hl]
    ld [CurPartySpecies], a

    push af
    ld a, MON_LEVEL
    call GetPartyParamLocation
    ld a, [hl]
    ld [CurPartyLevel], a

    ld b, 0
    ld de, wd002 + 1
; Based on GetEggMove in engine/breeding/egg.asm:
.loop
    ld a, [CurPartySpecies]
    dec a
    push bc
    ld b, 0
    ld c, a
    ld hl, EvosAttacksPointers
rept 2
    add hl, bc
endr
    ld a, BANK(EvosAttacksPointers)
    call GetFarHalfword
.skip_evos:
    ld a, BANK(EvosAttacks)
    call GetFarByte
    inc hl
    and a
    jr nz, .skip_evos

.loop_moves:
    ld a, BANK(EvosAttacks)
    call GetFarByte
    inc hl
    and a
    jr z, .done
    ld c, a
    ld a, [CurPartyLevel]
    cp c
    ld a, BANK(EvosAttacks)
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
    ld a, $FF
    ld [de], a
    pop bc
    inc b
    push bc
    jr .loop_moves

.done:
    pop bc
    pop af
    ld [CurPartySpecies], a
    ld a, b
    ld [wd002], a
    and a
    ret

;--------------------------------------------------------------
; GetEggRemindableMoves is similar to GetRemindableMoves, with minor differences.
;--------------------------------------------------------------
GetEggRemindableMoves:
    ld hl, wd002
    xor a
    ld [hli], a
    ld [hl], $FF

    ld a, MON_SPECIES
    call GetPartyParamLocation
    ld a, [hl]
    ld [CurPartySpecies], a

    push af
    ld a, MON_LEVEL
    call GetPartyParamLocation
    ld a, [hl]
    ld [CurPartyLevel], a

    ld b, 0
    ld de, wd002 + 1

    ld a, [CurPartySpecies]
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
    ld a, $FF
    ld [de], a
    pop bc
    inc b
    push bc
    jr .loop_moves

.done:
    pop bc
    pop af
    ld [CurPartySpecies], a
    ld a, b
    ld [wd002], a
    and a
    ret

;--------------------------------------------------------------
; CheckAlreadyInList and CheckPokemonAlreadyKnowsMove remain unchanged.
;--------------------------------------------------------------
CheckAlreadyInList:
    push hl
    ld hl, wd002 + 1
.loop:
    ld a, [hli]
    cp $FF
    jr z, .nope
    cp c
    jr nz, .loop
    pop hl
    scf
    ret
.nope:
    pop hl
    and a
    ret

CheckPokemonAlreadyKnowsMove:
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

;--------------------------------------------------------------
; ChooseMoveToLearn (mostly unchanged)
;--------------------------------------------------------------
ChooseMoveToLearn:
    call FadeToMenu
    callba BlankScreen
    call UpdateSprites
    ld hl, .MenuDataHeader
    call CopyMenuDataHeader
    xor a
    ld [wMenuCursorBuffer], a
    ld [wMenuScrollPosition], a
    call ScrollingMenu
    call SpeechTextBox
    ld a, [wMenuJoypad]
    cp B_BUTTON
    jr z, .carry
    ld a, [MenuSelection]
    ld [wPutativeTMHMMove], a
    and a
    ret
.carry:
    scf
    ret

.MenuDataHeader:
    db $40                ; flags
    db 01, 01             ; start coords
    db 11, 19             ; end coords
    dw .menudata2
    db 1                  ; default option

.menudata2:
    db $30                ; pointers
    db 5, 8              ; rows, columns
    db 1                 ; horizontal spacing
    dbw 0, wd002
    dba .PrintMoveName
    dba .PrintDetails
    dba .PrintMoveDesc

.PrintMoveName:
    push de
    ld a, [MenuSelection]
    ld [wd265], a
    call GetMoveName
    pop hl
    call PlaceString
    ret

;--------------------------------------------------------------
; .PrintDetails – Revised for merged move type/category.
;--------------------------------------------------------------
.PrintDetails:
    ld hl, StringBuffer1
    ld bc, StringBuffer2 - StringBuffer1
    ld a, " "           ; fill with a space
    call ByteFill

    ld a, [MenuSelection]
    cp $FF
    ret z
    push de
    dec a
    ld bc, MOVE_LENGTH        ; MOVE_LENGTH should be 7 now.
    ld hl, Moves
    call AddNTimes            ; HL now points to the selected move entry.
    ld [wdTempWord], hl       ; save move pointer in wdTempWord

    ;--- Extract the combined move type/category field from offset 3 ---
    ld hl, Moves + 3          ; offset 3 holds the combined field.
    call AddNTimes            ; adjust HL for the selected move (if required)
    ld hl, [wdTempWord]       ; reload the move pointer
    add hl, 3
    ld a, [hl]               ; A = combined move field
    push af                ; save combined field

    ;--- Extract and print move category ---
    ld hl, [wdTempWord]
    add hl, 3
    ld a, [hl]
    and $F0                ; isolate upper nibble (move category bits)
    swap a                 ; swap nibbles so that category index is in lower nibble
    add a                  ; a = a * 2
    add a                  ; now a = a * 4 (assuming each class abbreviation is 4 bytes)
    ld b, 0
    ld c, a
    ld hl, .Classes         ; pointer to category strings (e.g. "Phys@", "Spcl@", "Stat@")
    add hl, bc
    ld de, StringBuffer1
    ld bc, 4
    call CopyBytes          ; copy category abbreviation into buffer
    ld hl, StringBuffer1 + 4
    ld [hl], "/"           ; append slash

    ;--- Extract and print move type ---
    pop af                 ; retrieve combined field
    and $0F               ; isolate lower nibble (move type index)
    add a                 ; multiply by 2
    add a                 ; now a = (move type index)*4 (if each type is 4 bytes)
    ld b, 0
    ld c, a
    ld hl, .Types         ; pointer to type strings (e.g. "Normal@", " Fight@", etc.)
    add hl, bc
    ld de, StringBuffer1 + 5
    ld bc, 4
    call CopyBytes          ; copy type abbreviation into buffer
    ld hl, StringBuffer1 + 9
    ld [hl], "/"           ; append slash

    ;--- Print move power (stored at offset 2) ---
    ld hl, [wdTempWord]
    add hl, 2
    ld a, [hl]
    ld [EngineBuffer1], a
    ld hl, StringBuffer1 + 10
    ld de, EngineBuffer1
    ld bc, 2
    call PrintNum

    ;--- Print move PP (stored at offset 5) ---
    ld hl, [wdTempWord]
    add hl, 5
    ld a, [hl]
    ld [wdTempByteValue], a
    ld hl, StringBuffer1 + 12
    ld de, wdTempByteValue
    ld bc, 2
    call PrintNum

    ld hl, StringBuffer1 + 14
    ld [hl], "@"
    pop hl
    ld de, StringBuffer1
    jp PlaceString

.Types:
    db "Normal@", " Fight@", "Flying@", "Poison@", "Ground@", "  Rock@", "   Bug@", " Ghost@"
    db " Steel@", "  Fire@", " Water@", " Grass@", "Electr@", "Psychc@", "   Ice@", "Dragon@"
    db "  Dark@", " Fairy@", "   ???@"

.Classes:
    db "Phys@", "Spcl@", "Stat@"

.PrintMoveDesc:
    push de
    call SpeechTextBox
    ld a, [MenuSelection]
    inc a
    pop de
    ret z
    ld [CurSpecies], a
    hlcoord 1, 14
    predef PrintMoveDesc
    ret

Text_MoveReminderIntro:
    text_jump MoveReminderIntroText
    db "@"

Text_MoveReminderPrompt:
    text_jump MoveReminderPromptText
    db "@"

Text_MoveReminderWhichMon:
    text_jump MoveReminderWhichMonText
    db "@"

Text_MoveReminderWhichMove:
    text_jump MoveReminderWhichMoveText
    db "@"

Text_MoveReminderCancel:
    text_jump MoveReminderCancelText
    db "@"

Text_MoveReminderEgg:
    text_jump MoveReminderEggText
    db "@"

Text_MoveReminderNoGoldLeaf:
    text_jump MoveReminderNoGoldLeafText
    db "@"

Text_MoveReminderNoMon:
    text_jump MoveReminderNoMonText
    db "@"

Text_MoveReminderNoMoves:
    text_jump MoveReminderNoMovesText
    db "@"
