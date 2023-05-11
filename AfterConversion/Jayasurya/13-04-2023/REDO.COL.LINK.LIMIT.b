* @ValidationCode : MjoxNTY0NTYxNzkyOkNwMTI1MjoxNjgxMzAzMzYxNjc4OklUU1NCTkc6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAyMTA4LjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 12 Apr 2023 18:12:41
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
* Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*12-04-2023            CONVERSION TOOL                AUTO R22 CODE CONVERSION           VM TO @VM
*12-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES
SUBROUTINE REDO.COL.LINK.LIMIT
*-----------------------------------------------------------------------------
* Subroutine Type : ROUTINE
* Attached to     : COLLATERAL
* Attached as     : AUTH ROUTINE
* Primary Purpose : LINK A COLLATERAL.RIGTH WITH COLLATERAL
* Incoming:
* ---------
*
* Outgoing:
* ---------
*
* Error Variables:
*
*-----------------------------------------------------------------------------
* Modification History:
*
* Development for : Asociacion Popular de Ahorros y Prestamos
* Development by  : Jorge Valarezo - TAM Latin America
* Date            : February 25 2013
*
*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.COLLATERAL
    $INSERT I_F.COLLATERAL.RIGHT

    GOSUB INITIALISE
    GOSUB OPEN.FILES
    GOSUB PROCESS
RETURN

* =========
INITIALISE:
* =========
    FN.COLLATERAL.RIGHT = 'F.COLLATERAL.RIGHT'
    F.COLLATERAL.RIGHT = ''
    R.COLLATERAL.RIGHT = ''

    Y.ID.LOAN.NAME = 'L.AC.LK.COL.ID'
    Y.ID.LOAN.POS  = ''


    LOC.REF.APPL = 'COLLATERAL'
    LOC.REF.FIELDS = Y.ID.LOAN.NAME
    LOC.REF.POS = ''

    CALL MULTI.GET.LOC.REF(LOC.REF.APPL,LOC.REF.FIELDS,LOC.REF.POS)
    Y.ID.LOAN.POS = LOC.REF.POS<1,1>

    LOAN.AA.ID = ''
    Y.AA.ID<1> = 'AA.ID'
    Y.POS = ''
RETURN

* =========
OPEN.FILES:
* =========
*
    CALL OPF(FN.COLLATERAL.RIGHT,F.COLLATERAL.RIGHT)
*
RETURN
* ======
PROCESS:
* ======

    LOAN.AA.ID   = R.NEW(COLL.LOCAL.REF)<1,Y.ID.LOAN.POS>
    Y.AA.ID<2> = LOAN.AA.ID
    COLL.RIGHT.ID = ID.NEW['.',1,2]
    CALL REDO.COL.GET.LIMIT.AA(Y.AA.ID,Y.LIMIT.ID)

    IF Y.LIMIT.ID EQ 'ERROR' THEN
        RETURN
    END
    CALL F.READ (FN.COLLATERAL.RIGHT,COLL.RIGHT.ID,R.COLLATERAL.RIGHT,F.COLLATERAL.RIGHT,Y.ERR)
    IF Y.ERR THEN
        RETURN
    END

    LOCATE Y.LIMIT.ID IN R.COLLATERAL.RIGHT<COLL.RIGHT.LIMIT.REFERENCE,1> SETTING Y.POS ELSE
        Y.NUM.LIMT = DCOUNT(R.COLLATERAL.RIGHT,@VM)
        Y.NUM.LIMIT+=1
        R.COLLATERAL.RIGHT<COLL.RIGHT.LIMIT.REFERENCE,Y.NUM.LIMIT> = Y.LIMIT.ID
        CALL F.WRITE(FN.COLLATERAL.RIGHT,COLL.RIGHT.ID,R.COLLATERAL.RIGHT)

    END
RETURN

END
