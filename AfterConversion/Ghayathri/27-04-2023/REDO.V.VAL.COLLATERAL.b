* @ValidationCode : MjoxNzM3OTMxMjA2OkNwMTI1MjoxNjgyNDkyNzI0MzQyOmhhaTotMTotMTowOjA6ZmFsc2U6Ti9BOlIyMV9BTVIuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 26 Apr 2023 12:35:24
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : hai
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
$PACKAGE APAP.AA ;*MANUAL R22 CODE CONVERSTION
SUBROUTINE REDO.V.VAL.COLLATERAL
    

*--------------------------------------------------------------------------
*Company Name: ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
*Program Name: REDO.V.VAL.COLLATERAL
*----------------------------------------------------------------------------
*Description:
*----------------------------------------------------------------------------
*       This routine is used to check the value in the secured field in limit
* for the value of collateral in term.amount
*----------------------------------------------------------------------------
*Modification History:
*-----------------------------------------------------------------------------
*
*  DATE             WHO       REFERENCE             DESCRIPTION
* 29-06-2010      PREETHI.MD   ODR-2009-10-0326 N.3  INITIAL CREATION
*
* 15-03-2011     Ravikiran AV    PACS00034161         Get the Limit ID stored while proceessing the LIMIT record
* DATE                 WHO                  REFERENCE                    DESCRIPTION
* 29/03/2023         SURESH      MANUAL R22 CODE CONVERSTION           Package Name added APAP.AA
* 29/03/2023         Conversion Tool      AUTO R22 CODE CONVERSTION                 FM TO @FM,VM TO@VM
*-----------------------------------------------------------------------------------
*----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.AA.TERM.AMOUNT
    $INSERT I_F.AA.LIMIT
    $INSERT I_AA.LOCAL.COMMON
    $INSERT I_F.COLLATERAL
    $INSERT I_GTS.COMMON
    $INSERT I_System


    GOSUB INIT
    GOSUB PROCESS
RETURN

*------------------------------------------------------------------------
INIT:
*------------------------------------------------------------------------

    ARR.ID=c_aalocArrId
    EFF.DATE=TODAY
    PROP.CLASS='LIMIT'
    PROPERTY=''
    R.CONDITION=''
    ERR.MSG=''

    FN.COLLATERAL="F.COLLATERAL"
    F.COLLATERAL=""
    R.COLLATERAL=""
    FN.ARR.LIMIT = 'F.AA.ARR.LIMIT'
    F.ARR.LIMIT = ''
    CALL OPF (FN.ARR.LIMIT, F.ARR.LIMIT)

RETURN

*-------------------------------------------------------------------------
PROCESS:
*-------------------------------------------------------------------------


    CALL APAP.AA.REDO.CRR.GET.CONDITIONS(ARR.ID,EFF.DATE,PROP.CLASS,PROPERTY,R.CONDITION,ERR.MSG)



*PACS00034161


*   LIMIT.ID = System.getVariable('CURRENT.LIMIT.ID')

*    CALL CACHE.READ(FN.ARR.LIMIT, LIMIT.ID, R.CONDITION, RET.ERR)
*PACS00034161

    LOC.REF.APPL="AA.PRD.DES.LIMIT":@FM:"AA.PRD.DES.TERM.AMOUNT" ;*AUTO R22 CODE CONVERSION
    LOC.REF.FIELDS="L.AA.SECURED":@FM:"L.AA.COL":@VM:"L.AA.COL.VAL":@VM:"L.AA.COL.DESC" ;*AUTO R22 CODE CONVERSION
    LOC.REF.POS=""
    CALL MULTI.GET.LOC.REF(LOC.REF.APPL,LOC.REF.FIELDS,LOC.REF.POS)

    Y.SEC.POS=LOC.REF.POS<1,1>
    Y.COL.POS=LOC.REF.POS<2,1>
    Y.COLVAL.POS=LOC.REF.POS<2,2>
    Y.COLDESC.POS=LOC.REF.POS<2,3>
    Y.SECURED=R.CONDITION<AA.LIM.LOCAL.REF><1,Y.SEC.POS> 1
    Y.COLLATERAL=R.NEW(AA.AMT.LOCAL.REF)<1,Y.COL.POS>


    IF Y.COLLATERAL NE '' AND Y.SECURED EQ 'NO' THEN
        AF=AA.AMT.LOCAL.REF
        AV=Y.COL.POS
        ETEXT="AA-COLLATERAL.SEC"
        CALL STORE.END.ERROR
    END

    IF Y.COLLATERAL EQ '' AND Y.SECURED EQ 'YES' THEN
        AF=AA.AMT.LOCAL.REF
        AV=Y.COL.POS
        ETEXT="AA-COLLATERAL.UNSEC"
        CALL STORE.END.ERROR
    END

    IF Y.COLLATERAL NE "" THEN
        CALL OPF(FN.COLLATERAL,F.COLLATERAL)
        CALL F.READ(FN.COLLATERAL,Y.COLLATERAL,R.COLLATERAL,F.COLLATERAL,Y.ERR)
        IF R.COLLATERAL THEN
            R.NEW(AA.AMT.LOCAL.REF)<1,Y.COLVAL.POS>=R.COLLATERAL<COLL.NOMINAL.VALUE>
            R.NEW(AA.AMT.LOCAL.REF)<1,Y.COLDESC.POS>=R.COLLATERAL<COLL.DESCRIPTION>

        END
    END

RETURN

END
