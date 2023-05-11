$PACKAGE APAP.REDOENQ
SUBROUTINE REDO.E.BLD.RBHP.CAMPAIGN(Y.ENQ.DTLS)
*-----------------------------------------------------------------------------
* Company Name : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* Developed By : Pradeep M
* Program Name : REDO.E.BLD.RBHP.CAMPAIGN
*-----------------------------------------------------------------------------
* Description :Bulit routine to assign value to set variable.
* Linked with :
* In Parameter :
* Out Parameter :
*
**DATE           ODR                   DEVELOPER               VERSION
* 10-11-2011    ODR2011080055          Pradeep M
* 10-APRIL-2023      Conversion Tool       R22 Auto Conversion  - FM to @FM ,SM to @SM 
* 10-APRIL-2023      Harsha                R22 Manual Conversion - No changes 
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.CR.OPPORTUNITY
    $INSERT I_F.USER
    $INSERT I_F.CUSTOMER

    GOSUB OPEN.PROCESS
    GOSUB PROCESS

RETURN

OPEN.PROCESS:
*------------

    FN.CR.OPPORTUNITY='F.CR.OPPORTUNITY'
    F.CR.OPPORTUNITY=''

    CALL OPF(FN.CR.OPPORTUNITY,F.CR.OPPORTUNITY)

    FN.USER='F.USER'
    F.USER=''

    CALL OPF(FN.USER,F.USER)

    FN.CUSTOMER = 'F.CUSTOMER'
    F.CUSTOMER = ''
    CALL OPF(FN.CUSTOMER,F.CUSTOMER)

RETURN

PROCESS:
*-------

    Y.DPT.OFFCR=R.USER<EB.USE.DEPARTMENT.CODE>
    SEL.CMD="SELECT ":FN.CR.OPPORTUNITY
    CALL EB.READLIST(SEL.CMD,SEL.LIST,'',NO.OF.REC,ERR.SLIST)

    LOOP
        REMOVE Y.ID FROM SEL.LIST SETTING ID.POS
    WHILE Y.ID:ID.POS
        CALL F.READ(FN.CR.OPPORTUNITY,Y.ID,R.CR.OPPORTUNITY,F.CR.OPPORTUNITY,E.CR.OPPORTUNITY)
        Y.CR.CUSTOMER = R.CR.OPPORTUNITY<CR.OP.CUSTOMER>
        CALL F.READ(FN.CUSTOMER,Y.CR.CUSTOMER,R.CUSTOMER,F.CUSTOMER,E.CUSTOMER)
        Y.ACCOUNT.OFFICER = R.CUSTOMER<EB.CUS.ACCOUNT.OFFICER>
        IF Y.DPT.OFFCR EQ Y.ACCOUNT.OFFICER THEN
            Y.ACCOUNT.OFFICER.LIST<-1> = Y.ID
        END
    REPEAT
    CHANGE @FM TO @SM IN Y.ACCOUNT.OFFICER.LIST
    Y.ENQ.DTLS<2,1> = '@ID'
    Y.ENQ.DTLS<3,1> = 'EQ'
    Y.ENQ.DTLS<4,1> = Y.ACCOUNT.OFFICER.LIST

RETURN

END
