* @ValidationCode : MjotMTQ4MDQyNzgyMzpDcDEyNTI6MTY4MTEyMjkzMTczMDpIYXJpc2h2aWtyYW1DOi0xOi0xOjA6MDpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 10 Apr 2023 16:05:31
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : HarishvikramC
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOCHNLS
SUBROUTINE REDO.ARC.REPRINT.RECEIPTS(ROU.ARGS,ROU.RESPONSE,RESPONSE.TYPE,STYLE.SHEET)
*-----------------------------------------------------------------------------
*Company   Name    : APAP
*Developed By      : Martin Macias
*Program   Name    : REDO.ARC.REPRINT.RECEIPTS
*-----------------------------------------------------------------------------
*MODIFICATION HISTORY:
*
* 10-APR-2023     Conversion tool    R22 Auto conversion       IF condition added
* 10-APR-2023      Harishvikram C   Manual R22 conversion      CALL Routine format modified
*-----------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_System

    Y.TXN.DETAILS = System.getVariable("CURRENT.REPRN.VER")
    IF E EQ "EB-UNKNOWN.VARIABLE" THEN ;*R22 Auto conversion - START
        Y.TXN.DETAILS = ""
    END					;*R22 Auto conversion - END
    PDF.VER.ID = FIELD(Y.TXN.DETAILS," ",1)
    PDF.REC.ID = FIELD(Y.TXN.DETAILS," ",3)

    HEADER.FLD = ''
    DATA.FLD = ''
    FOOTER.FLD = ''
    Y.XSL = ''

    CALL APAP.REDOCHNLS.AI.REDO.PRINT.RECEIPTS(PDF.VER.ID,PDF.REC.ID,HEADER.FLD,DATA.FLD,FOOTER.FLD,Y.XSL) ;*Manual R22 conversion

    IF Y.XSL EQ '' THEN
        E = "AI-PRINT.PDF"
        RETURN
    END

    PDF.HEADER = HEADER.FLD
    PDF.DATA = DATA.FLD
    PDF.FOOTER = FOOTER.FLD

    RESPONSE.TYPE = 'XML.ENQUIRY'
    STYLE.SHEET = '/transforms/dummy.xsl'
    Y.LEN=LEN(PDF.DATA)
    Y.RES=PDF.DATA[0,Y.LEN-3]

    XML.RECS = '<window><panes><pane><dataSection><enqResponse>'
    XML.RECS :='<r><c><cap>' : PDF.HEADER : PDF.DATA : PDF.FOOTER : '^^IMAGE=apaplogo.jpg^^PAGE=':Y.XSL:' </cap></c></r>'
    XML.RECS :='</enqResponse></dataSection></pane></panes></window>'
    ROU.RESPONSE = XML.RECS
RETURN
END
