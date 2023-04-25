*-----------------------------------------------------------------------------
* <Rating>-20</Rating>
*-----------------------------------------------------------------------------
  SUBROUTINE REDO.R.BCR.REPORT.GEN.LIST.GET(Y.BCR.LIST)
*-----------------------------------------------------------------------------
* INTERFACE : REDO.BCR.REPORT : Buro de Credito
*
* This routines get from F.LOCKING the entry COB*BCR*REDO.INTERFACE.PARAM
* That is the list of ID's from REDO.INTERFACE.PARAM to process
* Finally the rourintes change VM to FM and returns the content
* Previously, some job had to have called to REDO.R.BCR.REPORT.GEN.LIST.BUILD
* @author youremail@temenos.com
* @stereotype subroutine
* @package infra.eb
* @parameters
*             Y.BCR.LIST   (in)  lista de identificador que deben procesarse
*-----------------------------------------------------------------------------
* Date       Name              Reference                     Version
* --------   ----              ----------                    --------
* 17.04.12   hpasquel          PACS00191153                  C.22 problems, improve COB
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.LOCKING

*-----------------------------------------------------------------------------
  GOSUB INITIALISE
  GOSUB PROCESS
  RETURN

*-----------------------------------------------------------------------------
PROCESS:
*-----------------------------------------------------------------------------
  R.LOCKING = ''
  Y.LOKING.ID = "COB*BCR*REDO.INTERFACE.PARAM"
  Y.ERR = ''
*
  IF NOT(F.LOCKING) THEN
    FN.LOCKING = 'F.LOCKING'
    F.LOCKING = ''
    CALL OPF(FN.LOCKING, F.LOCKING)
  END
  CALL F.READ(FN.LOCKING    ,Y.LOKING.ID  ,R.LOCKING     ,F.LOCKING     ,Y.ERR)

  IF Y.ERR EQ '' THEN
    Y.BCR.LIST = R.LOCKING<EB.LOK.CONTENT>   ;*Tus S/E
    Y.BCR.LIST = CHANGE(Y.BCR.LIST, VM, FM)
  END ELSE
    CALL OCOMO('REGISTRO ' : "COB*BCR*REDO.INTERFACE.PARAM" : "NO ENCONTRADO EN F.LOCKING")
    Y.BCR.LIST = ''
  END

  RETURN

*-----------------------------------------------------------------------------
INITIALISE:
*-----------------------------------------------------------------------------

  RETURN

*-----------------------------------------------------------------------------
END
