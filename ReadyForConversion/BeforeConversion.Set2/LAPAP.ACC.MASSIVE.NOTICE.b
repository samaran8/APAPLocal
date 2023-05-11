*========================================================================
*-----------------------------------------------------------------------------
* <Rating>-2</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE LAPAP.ACC.MASSIVE.NOTICE(SEL.LIST)
*========================================================================
* Technical report:
* =================
* Company Name   : APAP
* Program Name   : LAPAP.ACC.MASSIVE.NOTICE
* Date           : 2019-05-21
* Item ID        : --------------
*========================================================================
* Brief description :
* -------------------
* This program allow ....
*========================================================================
* Modification History :
* ======================
* Date           Author            Modification Description
* -------------  -----------       ---------------------------
* 2019-05-21     Richard HC        Initial Development
*========================================================================
* Content summary :
* =================
* Table name     :F.ACCOUNT
* Auto Increment :N/A
* Views/versions :
* EB record      :N/A
* Routine        :LAPAP.ACC.MASSIVE.NOTICE
*========================================================================

    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_F.ACCOUNT
    $INSERT LAPAP.BP I_LAPAP.ACC.MASSIVE.NOTICE.COMMON

*   ACC.ID = SEL.LIST

    CALL OCOMO(SEL.LIST)

    F.ID = EREPLACE(SEL.LIST,",",VM)
    FILE.ARR = DCOUNT(F.ID,@VM)

    ACC.ID = F.ID<1,1>
    MSG = F.ID<1,2>
    COMMENT = F.ID<1,3>


    R.SS = "";
    CALL F.READ(FN.ACC,ACC.ID,R.ACC,F.ACC,ERR.ACC)
    CALL GET.LOC.REF("ACCOUNT","L.AC.NOTIFY.1",POS)
    CALL GET.LOC.REF("ACCOUNT","L.AC.NOTIFY.2",PO2)

    PREVIOUS.NOTICE = R.ACC<AC.LOCAL.REF,POS>
    M = DCOUNT(PREVIOUS.NOTICE,@SM)
    S = M+1

*   DEBUG

    APP = "ACCOUNT"
    ID = ACC.ID
    Y.FUNC = "I"
    R.SS<AC.LOCAL.REF,POS,S> = MSG
    R.SS<AC.LOCAL.REF,PO2,S> = COMMENT
*   CALL LAPAP.BUILD.OFS.LOAD(APP,Y.FUNC,ID,R.SS)

    IF ERR.ACC THEN ;* OR ID EQ 0 THEN

        IF ID NE 0 THEN
            ARR<1> = "LAPAP.ACC.MASSIVE.NOTICE.REJECTED.txt"
            ARR<2> = "NO SE PUDO MODIFICAR EL STATUS DE LA CUENTA.( ":ID:" ) ":ERR.ACC
            ARR<3> = "../interface/T24ACCNOTICE"
            CALL LAPAP.WRITE.FILE(ARR)
        END

    END ELSE
        CALL LAPAP.BUILD.OFS.LOAD(APP,Y.FUNC,ID,R.SS)
    END


    RETURN

END
