*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE PACS.CORR.AAA

	$INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.STMT.ENTRY
    $INSERT I_F.AA.ARRANGEMENT.ACTIVITY
	
	FN.AAA = 'F.AA.ARRANGEMENT.ACTIVITY$NAU'
    F.AAA = ''
    CALL OPF(FN.AAA, F.AAA)
	
	FN.ENTRY.HOLD='F.ENTRY.HOLD'
	FV.ENTRY.HOLD=''
    CALL OPF(FN.ENTRY.HOLD,FV.ENTRY.HOLD)

    ***FT.ID = "FT21077D16H4"
	
	FN.SL = "&SAVEDLISTS&"
    F.SL = ""
    CALL OPF(FN.SL, F.SL)
	
	SL.ID = "SL.PROB.FT"
	
	CALL F.READ(FN.SL, SL.ID, R.SL, F.SL, SL.ERR)
	
	LOOP
        REMOVE FT.ID FROM R.SL SETTING SL.POS
    WHILE INP.ID : SL.POS
	
		HOLD.ID = 'FT':FT.ID
			
		CALL F.READ(FN.ENTRY.HOLD,HOLD.ID,R.ENTRY.HOLD,FV.ENTRY.HOLD,READ.ERR)
			
		Y.ENT.CNT = COUNT(R.ENTRY.HOLD,FM)
		
		UPD.ENTRY.HOLD = ''
		
		AAA.ID = ''
		
		ENTRY = ''
		
		FOR ENT.IDX = 1 TO Y.ENT.CNT
		
			ENTRY = RAISE(R.ENTRY.HOLD<ENT.IDX>)
			
			AAA.ID = ENTRY<AC.STE.AA.ITEM.REF>["*",7,1]
			
		NEXT ENT.IDX
		
		R.AAA = ''
		
		IF AAA.ID THEN
		
			CALL F.READ(FN.AAA,AAA.ID,R.AAA,F.AAA,AAA.ERR)
			
			IF R.AAA THEN
			
				CALL F.DELETE(FN.AAA, AAA.ID)
				
				CALL JOURNAL.UPDATE("")
				
			END
			
		END
		
    REPEAT
	
RETURN	
