* @ValidationCode : MjotMTc0NDA3MTk5ODpDcDEyNTI6MTY4MTM4MjQ4OTgzMDpJVFNTQk5HOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMjEwOC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 13 Apr 2023 16:11:29
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSSBNG
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
$PACKAGE APAP.REDORETAIL
SUBROUTINE REDO.DS.GET.USER.LOGIN(VAR.USER.COMP.DESC)
*--------------------------------------------------------------------------------
*Company   Name    :Asociacion Popular de Ahorros y Prestamos
*Developed By      :S SUDHARSANAN
*Program   Name    :REDO.DS.GET.BENEF.NAME
*---------------------------------------------------------------------------------
* DESCRIPTION       :This program is used to get the account title value
* Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*13-04-2023           CONVERSION TOOL                AUTO R22 CODE CONVERSION                 F.READ TO CACHE.READ, IF BLOCK ADDED
*13-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES

* ----------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.ACCOUNT.CLOSURE
    $INSERT I_F.AZ.ACCOUNT
    $INSERT I_System
    $INSERT I_F.COMPANY

    GOSUB PROCESS

RETURN

*********
PROCESS:
**********
    FN.COMPANY = 'F.COMPANY'
    F.COMPANY = ''
    CALL OPF(FN.COMPANY,F.COMPANY)


    CURRENT.COMP = System.getVariable("CURRENT.USER.BRANCH")
    IF E EQ "EB-UNKNOWN.VARIABLE" THEN ;* AUTO R22 CODE CONVERSION START
        CURRENT.COMP = ""
    END ;* AUTO R22 CODE CONVERSION END
    IF CURRENT.COMP EQ "CURRENT.USER.BRANCH" THEN
        LOCATE 'EB-UNKNOWN.VARIABLE' IN E<1,1> SETTING POS THEN
            E = ''
        END
        RETURN
    END
    VAR.USER.LOGIN = CURRENT.COMP

    CALL CACHE.READ(FN.COMPANY, VAR.USER.LOGIN, R.COMP, COMP.ERR) ;*AUTO R22 CODE CONVERSION

    VAR.USER.COMP.DESC = R.COMP<EB.COM.COMPANY.NAME,LNGG>

    IF NOT(VAR.USER.COMP.DESC) THEN
        VAR.USER.COMP.DESC = R.COMP<EB.COM.COMPANY.NAME,1>
    END

RETURN
*-----------------
END
