*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE PACS.FT.DELETE

	$INSERT I_COMMON
	$INSERT I_EQUATE
	$INSERT I_F.FUNDS.TRANSFER
	
	FN.FT = 'F.FUNDS.TRANSFER$NAU'
	F.FT = ''
	CALL OPF(FN.FT, F.FT)
	
	FN.SL = "&SAVEDLISTS&"
    F.SL = ""
    CALL OPF(FN.SL, F.SL)
	
	SL.ID = "SL.PROB.FT"
	
	CALL F.READ(FN.SL, SL.ID, R.SL, F.SL, SL.ERR)
	
	***FT.ID = "FT21077D16H4"
	
	LOOP
        REMOVE FT.ID FROM R.SL SETTING SL.POS
    WHILE INP.ID : SL.POS
	
		CALL F.READ(FN.FT,FT.ID,R.FT,F.FT,FT.ERR)
		
		IF R.FT THEN
		
			SAVE.ID.COMPANY = ID.COMPANY
			CALL LOAD.COMPANY(R.FT<FT.CO.CODE>)
			CALL F.DELETE(FN.FT, FT.ID)
			
			CALL JOURNAL.UPDATE("")
			
		END
		
    REPEAT
	
	
RETURN
