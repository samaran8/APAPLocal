*-----------------------------------------------------------------------------
* <Rating>-10</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE L.APAP.MOD.PR.OVERR
*----------------------------------------------------------------------------------------------------
* Description           : Esta rutina genera un mensaje override cuando se dispara cualquier activiad de AAA
*                         Atachada a la version : AA.ARRANGEMENT.ACTIVITY,L.APAP.VER.MANTPREST.RRHH
* Developed On          : 13-12-2021
* Developed By          : APAP
* Development Reference : MDR-1112
*----------------------------------------------------------------------------------------------------

    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_F.AA.ARRANGEMENT.ACTIVITY

    GOSUB PROCESO

    RETURN

PROCESO:
    CURR.NO = DCOUNT(R.NEW(AA.ARR.ACT.CURR.NO),VM)
    VAR.OVERRIDE.ID  = 'L.APAP.RHH.AUTH'
    TEXT    = VAR.OVERRIDE.ID
    CALL STORE.OVERRIDE(CURR.NO + 1)
    RETURN

END
