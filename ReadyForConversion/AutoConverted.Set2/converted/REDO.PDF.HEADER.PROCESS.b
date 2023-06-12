SUBROUTINE REDO.PDF.HEADER.PROCESS

*-----------------------------------------------------------------------------

*Company   Name    : APAP
*Developed By      : Temenos Application Management
*Program   Name    : REDO.PDF.HEADER.PROCESS

*-----------------------------------------------------------------------------

*---------------------------------------------------------------------------------
*MODIFICATION:
*---------------------------------------------------------------------------------
*DATE           ODR                   DEVELOPER               VERSION
*--------       ----------------      -------------           --------------------
*07.04.2011     PACS00036498           Prabhu N            INITIAL CREATION
*---------------------------------------------------------------------------------
*Insert Files
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_System

*-----------------------------------------------------------------------------

    PDF.HEADER = O.DATA
    CALL System.setVariable("CURRENT.SCA.PDF.HEADER",PDF.HEADER)
RETURN
END