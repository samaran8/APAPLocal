* @ValidationCode : MjoyMjg2MjI2NTpDcDEyNTI6MTY4MTgyOTA5Mzk2NjpJVFNTOi0xOi0xOjM3MzoxOmZhbHNlOk4vQTpERVZfMjAyMTA4LjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 18 Apr 2023 20:14:53
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : 373
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDORETAIL
SUBROUTINE REDO.DS.CUS.BENEF(Y.CUS.ID)
*--------------------------------------------------------------------------------------------------------
*Company   Name    : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
*Developed By      : Temenos Application Management
*Program   Name    : REDO.DS.CUS.BENEF
*--------------------------------------------------------------------------------------------------------
*Description  : This is a conversion routine used to display Beneficiary(joint holder) customer of deposit
*Linked With  :
*In Parameter : NA
*Out Parameter: Y.ACCOUNT.NO

*--------------------------------------------------------------------------------------------------------
* Modification History :
*-----------------------
* Date           Who                  Reference           Description
* ------         ------               -------------       -------------
* 10 NOV 2011    Sudharsanan S        CR.18               Initial Creation
* 26 NOV 2013    Vignesh Kumaar R     PACS00261598        AZ Deposit deal slip alignment issue
* Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*13-04-2023           CONVERSION TOOL                AUTO R22 CODE CONVERSION                 NO CHANGES
*13-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES
*--------------------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.CUSTOMER
    $INSERT I_F.ACCOUNT
    $INSERT I_F.RELATION
    $INSERT I_F.AZ.ACCOUNT

*--------------------------------------------------------------------------------------------------------
    GOSUB OPEN.PARA
    GOSUB PROCESS.PARA
RETURN
*--------------------------------------------------------------------------------------------------------
*********
OPEN.PARA:
*********
    FN.CUSTOMER = 'F.CUSTOMER'
    F.CUSTOMER = ''
    CALL OPF(FN.CUSTOMER,F.CUSTOMER)

    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT = ''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

    FN.RELATION = 'F.RELATION'
    F.RELATION = ''
    CALL OPF(FN.RELATION,F.RELATION)

RETURN
*--------------------------------------------------------------------------------------------------------
************
PROCESS.PARA:
************
    Y.ACCOUNT.ID = ID.NEW
    GOSUB READ.ACCOUNT
    Y.REL.COUNT = DCOUNT(Y.REL.CODE,@VM)
    Y.REL.NO = 1
    LOOP
    WHILE Y.REL.NO LE Y.REL.COUNT
        Y.RELATION.CODE = R.ACCOUNT<AC.RELATION.CODE,Y.REL.NO>
        IF Y.RELATION.CODE GE 500 AND Y.RELATION.CODE LE 529 THEN
            IF NOT(Y.JOINT.HOLDER) THEN
                Y.JOINT.HOLDER = R.ACCOUNT<AC.JOINT.HOLDER,Y.REL.NO>
            END ELSE
                Y.JOINT.HOLDER = Y.JOINT.HOLDER:',':R.ACCOUNT<AC.JOINT.HOLDER,Y.REL.NO>
            END
        END
        Y.REL.NO +=1
    REPEAT

* Fix for PACS00261598 [AZ Deposit deal slip alignment issue]

    IF Y.CUS.ID EQ 'JOINT.CUS.1' THEN
        IF Y.JOINT.HOLDER THEN
            Y.CUS.ID = R.NEW(AZ.CUSTOMER)
        END
    END

    IF Y.CUS.ID EQ 'JOINT.CUS.2' THEN
        IF NOT(Y.JOINT.HOLDER) THEN
            Y.CUS.ID = R.NEW(AZ.CUSTOMER)
        END ELSE
            Y.CUS.ID = Y.JOINT.HOLDER
        END
    END

    IF Y.CUS.ID[1,5] EQ 'JOINT' THEN
        Y.CUS.ID = ''
    END

* End of Fix

RETURN
*--------------------------------------------------------------------------------------------------------
************
READ.ACCOUNT:
************
    R.ACCOUNT = ''
    ACCOUNT.ERR = ''
    CALL F.READ(FN.ACCOUNT,Y.ACCOUNT.ID,R.ACCOUNT,F.ACCOUNT,ACCOUNT.ERR)
    Y.REL.CODE = R.ACCOUNT<AC.RELATION.CODE>

RETURN
*--------------------------------------------------------------------------------------------------------
END
