*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE DR.REGN16.GET.CLAIM.STATUS
    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT TAM.BP I_F.REDO.ISSUE.CLAIMS

    STATUS.VAL = COMI
    CLAIM.STATUS = ''
    STATUS.1 = "DOCUMENT.PENDING":FM:"IN-PROCESS":FM:"OPEN"
    STATUS.2 = "CLOSED":FM:"RESOLVED":FM:"RESOLVED-NOTIFIED"
    PR.POS = ''
    LOCATE STATUS.VAL IN STATUS.1 SETTING PR.POS THEN
        CLAIM.STATUS = 'PR'
    END ELSE
        TR.POS = ''
        LOCATE STATUS.VAL IN STATUS.2 SETTING TR.POS THEN
            CLAIM.STATUS = 'TR'
        END
    END
    COMI = CLAIM.STATUS
    RETURN
END
