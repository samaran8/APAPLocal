* @ValidationCode : MjotMTAxODA3OTY1NjpDcDEyNTI6MTY4MjQxMjMzMzI2MzpIYXJpc2h2aWtyYW1DOi0xOi0xOjA6MTpmYWxzZTpOL0E6REVWXzIwMjEwOC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 25 Apr 2023 14:15:33
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : HarishvikramC
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOVER
SUBROUTINE REDO.V.AUT.CHK.EXST.CUST
*--------------------------------------------------------------------------------
*Company   Name    :Asociacion Popular de Ahorros y Prestamos
*Developed By      :PRABHU.N
*Program   Name    :REDO.V.AUT.CHK.EXST.CUST
*---------------------------------------------------------------------------------

*DESCRIPTION       :If CUSTOMER.STATUS field in CUSTOMER application is set DECEASED and
*                  when you authorize the record, this routine sets local field in ACCOUNT
*                  application ACCOUNT.STATUS2 to DECEASED
*LINKED WITH       :

* ----------------------------------------------------------------------------------
*Modification Details:
*=====================
*   Date               who           Reference            Description
* 29-JUN-2010        Prabhu.N       ODR-2009-10-0315       Initial Creation
* 28-FEB-2011        H GANESH      PACS00033051      Modified for Deaceased Status
* 01-AUG-2011        RAVIKIRAN     PACS00089338       Update Deposit when Customer is changed to DECEASED
*10-01-2012          Prabhu        PACS00172828      Update for Reversal as well
*-----------------------------------------------------------------------------------
*Modification History
*DATE                       WHO                         REFERENCE                DESCRIPTION
*06-04-2023           Conversion Tool          R22 Auto Code conversion      FM TO @FM,VM TO @VM ,SM TO @SM, ++ TO +=1
*06-04-2023            Samaran T                Manual R22 Code Conversion    No Changes
*--------------------------------------------------------------------------------------------------

    $INSERT I_EQUATE
    $INSERT I_COMMON
    $INSERT I_F.CUSTOMER
    $INSERT I_F.ACCOUNT
    $INSERT I_F.AZ.ACCOUNT
    GOSUB INIT
    GOSUB OPENFILE
    GOSUB PROCESS
RETURN
*----
INIT:
*----
    FN.ACCOUNT='F.ACCOUNT'
    F.ACCOUNT=''
    FN.CUSTOMER.ACCOUNT='F.CUSTOMER.ACCOUNT'
    F.CUSTOMER.ACCOUNT=''
    FN.JOINT.CONTRACTS.XREF='F.JOINT.CONTRACTS.XREF'
    F.JOINT.CONTRACTS.XREF=''
    FN.AZ.ACCOUNT='F.AZ.ACCOUNT'
    F.AZ.ACCOUNT = ''

    LREF.APP='ACCOUNT':@FM:'AZ.ACCOUNT'
    LREF.FIELD='L.AC.STATUS2':@FM:'L.AC.STATUS2'
    Y.LREF.POS=''
RETURN
*--------
OPENFILE:
*--------
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)
    CALL OPF(FN.CUSTOMER.ACCOUNT,F.CUSTOMER.ACCOUNT)
    CALL OPF(FN.JOINT.CONTRACTS.XREF,F.JOINT.CONTRACTS.XREF)
    CALL OPF(FN.AZ.ACCOUNT,F.AZ.ACCOUNT)
RETURN
*-------
PROCESS:
*--------



    Y.CUSTOMER.ID=ID.NEW
    Y.CUSTOMER.STATUS=R.NEW(EB.CUS.CUSTOMER.STATUS)
    Y.OLD.STATUS=R.OLD(EB.CUS.CUSTOMER.STATUS)
*SEL.CMD="SELECT " : FN.ACCOUNT : " WITH " : "CUSTOMER EQ " : Y.CUSTOMER.ID
*CALL EB.READLIST(SEL.CMD,SEL.LIST,'',NO.OF.REC,AC.ERR)

    CALL MULTI.GET.LOC.REF(LREF.APP,LREF.FIELD,Y.LREF.POS)

    LREF.POS=Y.LREF.POS<1>
    AZ.LREF.POS=Y.LREF.POS<2>
    IF Y.CUSTOMER.STATUS EQ "3" AND Y.OLD.STATUS NE "3" THEN
        GOSUB SET.DECEASED
    END

    IF Y.OLD.STATUS EQ "3" AND Y.CUSTOMER.STATUS NE "3" THEN
        GOSUB DEL.DECEASED
    END

RETURN
*------------
SET.DECEASED:
*------------

    CALL F.READ(FN.CUSTOMER.ACCOUNT,Y.CUSTOMER.ID,R.CUSTOMER.ACCOUNT,F.CUSTOMER.ACCOUNT,CUS.ACC.ERR)
    CHANGE @VM TO @FM IN R.CUSTOMER.ACCOUNT
    NO.OF.REC=DCOUNT(R.CUSTOMER.ACCOUNT,@FM)
    VAR1=1
    LOOP
    WHILE VAR1 LE NO.OF.REC
        CALL F.READ(FN.ACCOUNT,R.CUSTOMER.ACCOUNT<VAR1>,R.ACCOUNT,F.ACCOUNT,ACC.ERR)
        VAR.ACCOUNT = R.CUSTOMER.ACCOUNT<VAR1>
        IF R.ACCOUNT THEN
            GOSUB CHECK.DECEASED
        END
        VAR1 += 1
    REPEAT

    CALL F.READ(FN.JOINT.CONTRACTS.XREF,Y.CUSTOMER.ID,R.JOINT.HOLDER,F.JOINT.CONTRACTS.XREF,JOINT.ERR)
    CHANGE @VM TO @FM IN R.JOINT.HOLDER
    NO.OF.JNT.REC=DCOUNT(R.JOINT.HOLDER,@FM)

    VAR1=1
    LOOP
    WHILE VAR1 LE NO.OF.JNT.REC
        VAR.ACCOUNT = ''
        CALL F.READ(FN.ACCOUNT,R.JOINT.HOLDER<VAR1>,R.ACCOUNT,F.ACCOUNT,ACC.ERR)
        VAR.ACCOUNT = R.JOINT.HOLDER<VAR1>
        IF R.ACCOUNT THEN
            Y.JOINT.HOLDER=R.ACCOUNT<AC.JOINT.HOLDER>
            Y.RELATION.CODE=R.ACCOUNT<AC.RELATION.CODE>
            CHANGE @VM TO @FM IN Y.JOINT.HOLDER
            CHANGE @VM TO @FM IN Y.RELATION.CODE
            GOSUB CHECK.DECEASED1
        END

        VAR1 += 1
    REPEAT

RETURN
*--------------------
CHECK.DECEASED1:
*-------------------
    LOCATE Y.CUSTOMER.ID IN Y.JOINT.HOLDER SETTING JOINT.POS THEN
        IF Y.RELATION.CODE<JOINT.POS> GE 500 AND Y.RELATION.CODE<JOINT.POS> LE 509 THEN
            GOSUB CHECK.DECEASED
        END
    END
RETURN
*--------------------
CHECK.DECEASED:
*--------------------
    Y.STATUS2.LIST=R.ACCOUNT<AC.LOCAL.REF,LREF.POS>
    Y.STATUS2.LIST.SIZE=DCOUNT(Y.STATUS2.LIST,@SM)
    CHANGE @SM TO @FM IN Y.STATUS2.LIST

    LOCATE 'DECEASED' IN Y.STATUS2.LIST SETTING DEC.POS ELSE
        R.ACCOUNT<AC.LOCAL.REF,LREF.POS,Y.STATUS2.LIST.SIZE+1>='DECEASED'
        R.ACCOUNT<AC.POSTING.RESTRICT>  = '3'
        TEMP.V = V
        V = AC.AUDIT.DATE.TIME
        CALL F.LIVE.WRITE(FN.ACCOUNT,VAR.ACCOUNT,R.ACCOUNT)
        V = TEMP.V
        AZ.ACCOUNT.ID = VAR.ACCOUNT
        GOSUB UPDATE.AZ.ACCOUNT   ;*PACS00089338
    END

RETURN
*-------------------
UPDATE.AZ.ACCOUNT:
*-------------------

    IF R.ACCOUNT<AC.ALL.IN.ONE.PRODUCT> THEN        ;* Update the Deposit

        CALL F.READ(FN.AZ.ACCOUNT,AZ.ACCOUNT.ID,R.AZ.ACCOUNT,F.AZ.ACCOUNT,AZ.ACC.ERR)

        IF R.AZ.ACCOUNT THEN

            AZ.STATUS2.LIST=R.AZ.ACCOUNT<AZ.LOCAL.REF,AZ.LREF.POS>
            AZ.STATUS2.LIST.SIZE=DCOUNT(AZ.STATUS2.LIST,@SM)

            R.AZ.ACCOUNT<AZ.LOCAL.REF,AZ.LREF.POS,AZ.STATUS2.LIST.SIZE+1>='DECEASED'
            TEMP.V = V
            V = AZ.AUDIT.DATE.TIME
            CALL F.LIVE.WRITE(FN.AZ.ACCOUNT,AZ.ACCOUNT.ID,R.AZ.ACCOUNT)
            V = TEMP.V
*      CALL REDO.AZ.WRITE.TRACE("REDO.V.AUT.CHK.EXST.CUST",AZ.ACCOUNT.ID)
        END

    END

RETURN
*-----------
DEL.DECEASED:
*-----------
    CALL F.READ(FN.CUSTOMER.ACCOUNT,Y.CUSTOMER.ID,R.CUSTOMER.ACCOUNT,F.CUSTOMER.ACCOUNT,CUS.ACC.ERR)
    LOOP
        REMOVE Y.ACCOUNT.ID FROM R.CUSTOMER.ACCOUNT SETTING POS
    WHILE Y.ACCOUNT.ID:POS
        CALL F.READ(FN.ACCOUNT,Y.ACCOUNT.ID,R.ACCOUNT,F.ACCOUNT,ACC.ERR)
        IF R.ACCOUNT THEN
            GOSUB CHECK.DECEASED.EXIST
        END
    REPEAT

    CALL F.READ(FN.JOINT.CONTRACTS.XREF,Y.CUSTOMER.ID,R.JOINT.HOLDER,F.JOINT.CONTRACTS.XREF,JOINT.ERR)
    CHANGE @VM TO @FM IN R.JOINT.HOLDER
    NO.OF.JNT.REC=DCOUNT(R.JOINT.HOLDER,@FM)

    VAR1=1
    LOOP
    WHILE VAR1 LE NO.OF.JNT.REC
        Y.ACCOUNT.ID = ''
        CALL F.READ(FN.ACCOUNT,R.JOINT.HOLDER<VAR1>,R.ACCOUNT,F.ACCOUNT,ACC.ERR)
        Y.ACCOUNT.ID = R.JOINT.HOLDER<VAR1>
        IF R.ACCOUNT THEN
            Y.JOINT.HOLDER=R.ACCOUNT<AC.JOINT.HOLDER>
            Y.RELATION.CODE=R.ACCOUNT<AC.RELATION.CODE>
            CHANGE @VM TO @FM IN Y.JOINT.HOLDER
            CHANGE @VM TO @FM IN Y.RELATION.CODE
            GOSUB CHECK.DECEASED.EXIST1
        END

        VAR1 += 1
    REPEAT
RETURN
*------------------------------
CHECK.DECEASED.EXIST1:
*----------------------------
    LOCATE Y.CUSTOMER.ID IN Y.JOINT.HOLDER SETTING JOINT.POS THEN
        IF Y.RELATION.CODE<JOINT.POS> GE 500 AND Y.RELATION.CODE<JOINT.POS> LE 509 THEN
            GOSUB CHECK.DECEASED.EXIST
        END
    END
RETURN
*---------------------------------------
CHECK.DECEASED.EXIST:
*-------------------------------------
    Y.ACCOUNT.STATUS2=R.ACCOUNT<AC.LOCAL.REF,LREF.POS>
    CHANGE @SM TO @FM IN Y.ACCOUNT.STATUS2
    LOCATE 'DECEASED' IN Y.ACCOUNT.STATUS2 SETTING EXIST.POS THEN
        DEL R.ACCOUNT<AC.LOCAL.REF,LREF.POS,EXIST.POS>
        R.ACCOUNT<AC.POSTING.RESTRICT>  = ''
        TEMP.V = V
        V = AC.AUDIT.DATE.TIME
        CALL F.LIVE.WRITE(FN.ACCOUNT,Y.ACCOUNT.ID,R.ACCOUNT)
        V = TEMP.V
        CALL F.READ(FN.AZ.ACCOUNT,Y.ACCOUNT.ID,R.AZ.ACCOUNT,F.AZ.ACCOUNT,ACC.ERR)
        IF R.AZ.ACCOUNT THEN
            R.AZ.ACCOUNT<AZ.LOCAL.REF,AZ.LREF.POS>=R.ACCOUNT<AC.LOCAL.REF,LREF.POS>
            CALL F.WRITE(FN.AZ.ACCOUNT,Y.ACCOUNT.ID,R.AZ.ACCOUNT)
*      CALL REDO.AZ.WRITE.TRACE("REDO.V.AUT.CHK.EXST.CUST",Y.ACCOUNT.ID)
        END
    END
RETURN
*---------------------------------------------
END
