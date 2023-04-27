*-----------------------------------------------------------------------------
* <Rating>-21</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE LAPAP.GET.CAJERO.USER.RT
    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_F.TELLER.ID
    
    *--DEBUG
    
    Y.INP.USER.ID  = COMI
    Y.INP.USER.ID = Y.INP.USER.ID<1,1>
    Y.INP.USER.ID = FIELD(Y.INP.USER.ID, "_",2)

    *--Y.INP.USER.ID  = 'A.13498'
    
    GOSUB INIT
    GOSUB PROCESS

    COMI = Y.ID.CAJERO

    RETURN

*---------------
INIT:
*---------------

    Y.ID.CAJERO =''    
    FN.TELLER.ID = 'F.TELLER.ID'
    FV.TELLER.ID  = ""
    R.TELLER.ID = ""
    TELLER.ID_ERR = ""
    CALL OPF(FN.TELLER.ID,FV.TELLER.ID)

    RETURN

*---------------
PROCESS:
*---------------
    *--DEBUG

    SEL.CMD = 'SELECT ' : FN.TELLER.ID : ' WITH USER LIKE "' : Y.INP.USER.ID : '"'
    CALL EB.READLIST(SEL.CMD, SEL.LIST,"", NO.OF.REC, SEL.ERR)
    
    LOOP
        REMOVE Y.TELLER.ID FROM SEL.LIST SETTING RTE.POS
    WHILE Y.TELLER.ID DO
       IF NO.OF.REC EQ 1 THEN 
          Y.ID.CAJERO = Y.TELLER.ID
          RETURN 
       END ELSE 
           CALL F.READ(FN.TELLER.ID,Y.TELLER.ID,R.TELLER.ID,FV.TELLER.ID,TELLER.ID_ERR) 

           Y.ID.CAJERO = Y.TELLER.ID
           IF R.TELLER.ID<TT.TID.RECORD.STATUS> EQ 'OPEN'  THEN
              RETURN 
           END
       END    
 
    REPEAT 

    RETURN
END
