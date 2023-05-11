*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE L.APAP.EPOCH.CONV
    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_ENQUIRY.COMMON

    Y.DATETIME =O.DATA
    Y.FECHA.LEIBLE = OCONV(ICONV(Y.DATETIME[1,6],"D2/"),'D4Y'):Y.DATETIME[3,4]
    O.DATA = Y.FECHA.LEIBLE

    RETURN
END
