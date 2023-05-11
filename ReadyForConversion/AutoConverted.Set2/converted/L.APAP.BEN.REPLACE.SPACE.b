SUBROUTINE L.APAP.BEN.REPLACE.SPACE
*--------------------------------------------------------------------------------------------------
* Description           : Esta rutina reemplaza el caracter '~' por espacio
* Developed On          : ---
* Developed By          : Anthony Martinez
* Development Reference : ---
*--------------------------------------------------------------------------------------------------
*  M O D I F I C A T I O N S
* ***************************
* Defect Reference       Modified By                    Date of Change        Change Details
* --------               Anthony Martinez               26/11/2018            Creation
*--------------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.BENEFICIARY

    GOSUB INITIAL
    GOSUB PROCESS

INITIAL:

    Y.NICKNAME = R.NEW(ARC.BEN.NICKNAME)
    Y.ARC.BEN.LOCAL.REF = R.NEW(ARC.BEN.LOCAL.REF)

RETURN

PROCESS:

    R.NEW(ARC.BEN.NICKNAME) = EREPLACE(Y.NICKNAME, "~", " ")
    R.NEW(ARC.BEN.LOCAL.REF) = EREPLACE(Y.ARC.BEN.LOCAL.REF, "~", " ")

RETURN

END
