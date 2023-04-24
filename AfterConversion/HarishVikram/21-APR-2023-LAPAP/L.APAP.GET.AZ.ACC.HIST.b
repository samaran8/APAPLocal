* @ValidationCode : MjotMTE3NDE4OTUwNjpDcDEyNTI6MTY4MjA4MDE4NjA1OTpIYXJpc2h2aWtyYW1DOi0xOi0xOjA6MDpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 21 Apr 2023 17:59:46
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : HarishvikramC
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.LAPAP
SUBROUTINE L.APAP.GET.AZ.ACC.HIST(Y.FINAL)
*--------------------------------------------------------------------------------------------------
* Description           : Esta rutina retorma el historico de un certificado
* Developed On          : 27/05/2019
* Developed By          : Anthony Martinez
* Development Reference : ---
*--------------------------------------------------------------------------------------------------
*  M O D I F I C A T I O N S
* ***************************
* Defect Reference       Modified By                    Date of Change        Change Details
* --------               Anthony Martinez               27/05/2019            Creation
*
* 21-APR-2023     Conversion tool    R22 Auto conversion       BP Removed in insert file
* 21-APR-2023      Harishvikram C   Manual R22 conversion      No changes
*--------------------------------------------------------------------------------------------------
    $INSERT I_COMMON ;*R22 Auto conversion - start
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.STMT.ACCT.CR
    $INSERT I_F.ACCOUNT ;*R22 Auto conversion - end

*--Preparo la Tabla de Cuentas
    FN.ACCOUNT='F.ACCOUNT'
    F.ACCOUNT=''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

*--OBTENEMOS LOS CAMPOS ENVIADOS DESDE EL SS
    LOCATE "ACCOUNT" IN D.FIELDS<1> SETTING ACCOUNT.POS THEN
        Y.ACCOUNT = D.RANGE.AND.VALUE<ACCOUNT.POS>
    END

    LOCATE "DATE.FROM" IN D.FIELDS<1> SETTING DATE.FROM.POS THEN
        Y.DATE.FROM = D.RANGE.AND.VALUE<DATE.FROM.POS>
    END

    LOCATE "DATE.TO" IN D.FIELDS<1> SETTING DATE.TO.POS THEN
        Y.DATE.TO = D.RANGE.AND.VALUE<DATE.TO.POS>
    END

    Y.ARR.ID.SEL =  "'":Y.ACCOUNT:"'..."

*--Leo los datos de la cuenta para obtener la cuenta Asociada para los intereses liquidados
*--Para Buscar Los Movimientos de la misma e incluirlos en la presentacion
    R.ACCOUNT = ""; ERR=""
    CALL F.READ(FN.ACCOUNT, Y.ACCOUNT, R.ACCOUNT, F.ACCOUNT, ERR)

    Y.ARR.ID.INT = ""
    IF R.ACCOUNT THEN
        IF R.ACCOUNT<AC.INTEREST.LIQU.ACCT> NE "" THEN
            Y.ARR.ID.INT =  "'": R.ACCOUNT<AC.INTEREST.LIQU.ACCT> :"'..."
        END
    END

*SEL.CMD ="SELECT FBNK.STMT.ACCT.CR WITH @ID LIKE " : Y.ACCOUNT : "... AND CR.INT.DATE GE " : Y.DATE.FROM : " AND CR.INT.DATE LE " : Y.DATE.TO

*SEL.CMD = 'SELECT FBNK.STMT.ACCT.CR WITH @ID LIKE "' : Y.ARR.ID.SEL : '" AND CR.INT.DATE GE ' : Y.DATE.FROM : ' AND CR.INT.DATE LE ' : Y.DATE.TO
    SEL.CMD = 'SELECT FBNK.STMT.ACCT.CR WITH (@ID LIKE "' : Y.ARR.ID.SEL : '" OR @ID LIKE "' : Y.ARR.ID.INT : '") AND CR.INT.DATE GE ' : Y.DATE.FROM : ' AND CR.INT.DATE LE ' : Y.DATE.TO


    CALL EB.READLIST(SEL.CMD, SEL.LIST, "", NO.OF.REC, SEL.ERR)

*--TABLE OBJECTS
    FN.STMT = "F.STMT.ACCT.CR"; FV.STMT = ""; R.STMT = ""; STMT.ERR = ""
    CALL OPF(FN.STMT, FV.STMT)

    LOOP
        REMOVE Y.REC.ID FROM SEL.LIST SETTING RTE.POS
    WHILE Y.REC.ID DO

        CALL F.READ(FN.STMT, Y.REC.ID, R.STMT, FV.STMT, STMT.ERR)

        Y.TXN            = Y.REC.ID
        Y.TXN.DATE       = R.STMT<IC.STMCR.CR.INT.DATE>
        Y.TOTAL.INTEREST = R.STMT<IC.STMCR.TOTAL.INTEREST>
        Y.TOTAL.TAX      = R.STMT<IC.STMCR.TAX.FOR.CUSTOMER>
        Y.GRAND.TOTAL    = R.STMT<IC.STMCR.GRAND.TOTAL>

        Y.FINAL<-1> = Y.TXN :"|": Y.TXN.DATE :"|": Y.TOTAL.INTEREST :"|": Y.TOTAL.TAX :"|": Y.GRAND.TOTAL

    REPEAT

END
