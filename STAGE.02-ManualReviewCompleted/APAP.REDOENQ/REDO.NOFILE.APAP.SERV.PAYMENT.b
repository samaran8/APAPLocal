$PACKAGE APAP.REDOENQ
SUBROUTINE REDO.NOFILE.APAP.SERV.PAYMENT(LIST.PAY)
*-----------------------------------------------------------------------------
* Company Name : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* Developed By : Prabhu N
* Program Name : REDO.V.ACH.POP.DETAILS
*-----------------------------------------------------------------------------
* Description :
* Linked with :
* In Parameter : ENQ.DATA
* Out Parameter : None
*
**DATE           ODR                   DEVELOPER               VERSION
*
*12/11/11      PACS001464107           PRABHU N                   MODIFICATION
* 13-APRIL-2023      Conversion Tool       R22 Auto Conversion  - Added IF E EQ "EB-UNKNOWN.VARIABLE" THEN , F.READ to CACHE.READ , VM to @VM , ++ to +=
* 13-APRIL-2023      Harsha                R22 Manual Conversion - No changes
*-----------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.BENEFICIARY
    $INSERT I_System
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.FT.COMMISSION.TYPE
    $INSERT I_F.AI.REDO.ARCIB.PARAMETER


    GOSUB OPEN.FILES
    GOSUB FILL.DETAILS.FT
RETURN
*-----------*
OPEN.FILES:
*-----------*


    GET.ARC.CHG.CODE = ''
    APAP.ARC.SERVICE = ''
    APAP.ARC.AMT = ''
    Y.CHARGE.KEY = ''
    FN.ARCIB.PARAM='F.AI.REDO.ARCIB.PARAMETER'
    F.ARCIB.PARAM=''
    CALL OPF(FN.ARCIB.PARAM,F.ARCIB.PARAM)

    FN.FT.COMMISSION.TYPE='F.FT.COMMISSION.TYPE'
    F.FT.COMMISSION.TYPE=''
    CALL OPF(FN.FT.COMMISSION.TYPE,F.FT.COMMISSION.TYPE)
    GET.ARC.CHG.CODE = System.getVariable("CURRENT.ARC.CHG.CODE")
    IF E EQ "EB-UNKNOWN.VARIABLE" THEN 	;*R22 Auto Conversion  - Added IF E EQ "EB-UNKNOWN.VARIABLE" THEN
        GET.ARC.CHG.CODE = ""
    END

RETURN

*------------*
FILL.DETAILS.FT:
*------------*
    CALL CACHE.READ(FN.ARCIB.PARAM,'SYSTEM',R.ARC.REC,ARC.ERR)
    IF R.ARC.REC THEN
        APAP.ARC.SERVICE = R.ARC.REC<AI.PARAM.ARC.SERVICE>
        APAP.ARC.CHG.CODE = R.ARC.REC<AI.PARAM.ARC.CHARGE.CODE>

        TOT.APAP.SER = DCOUNT(APAP.ARC.SERVICE,@VM)
        CNT.SERVICE = 1
        LOOP

            REMOVE APAP.SERVICE FROM APAP.ARC.SERVICE SETTING APAP.SERV.POS
        WHILE CNT.SERVICE LE TOT.APAP.SER
            SERVICE.ID =R.ARC.REC<AI.PARAM.ARC.SERVICE,CNT.SERVICE>
            SERVICE.CODE = R.ARC.REC<AI.PARAM.ARC.CHARGE.CODE,CNT.SERVICE>
            CALL CACHE.READ(FN.FT.COMMISSION.TYPE, SERVICE.CODE, R.COMM.CODE, COMM.ERR)	;*R22 Auto Conversion  - F.READ to CACHE.READ
            IF R.COMM.CODE AND SERVICE.ID THEN

                SERVICE.AMT = R.COMM.CODE<FT4.FLAT.AMT>
                LIST.PAY<-1> = SERVICE.ID:"@":SERVICE.AMT:"@":SERVICE.CODE

            END
            CNT.SERVICE += 1
        REPEAT

    END



RETURN

END
