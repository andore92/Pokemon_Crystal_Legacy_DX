GetMoveCategoryName:
	; Copy the category name of move b to wStringBuffer1.
	
		ld a, b          ; b = move id
		dec a
		ld bc, MOVE_LENGTH
		ld hl, Moves + MOVE_CATEGORY ; get pointer to category
		call AddNTimes
		ld a, BANK(Moves)
		call GetFarByte  ; now A = category (0 = PHY, 1 = SPC, 2 = STA)
	
		ld hl, CategoryNames
		ld e, a
		ld d, 0
		add hl, de
		add hl, de       ; 2-byte pointer
		ld a, [hli]
		ld h, [hl]
		ld l, a
	
		ld de, wStringBuffer1
		ld bc, MOVE_NAME_LENGTH
		jp CopyBytes
	

INCLUDE "data/types/category_names.asm"