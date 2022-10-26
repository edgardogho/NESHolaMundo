MEMORY {
    ZP:     start = $00,    size = $0100, type = rw, file = "";
    HDR:    start = $0000,  size = $0010, type = ro, file = %O, fill = yes, fillval = $00;
    PRG:    start = $8000,  size = $8000, type = ro, file = %O, fill = yes, fillval = $00;
    CHR:    start = $0000,  size = $2000, type = ro, file = %O, fill = yes, fillval = $00;
}

SEGMENTS {
    ZEROPAGE: load = ZP,  type = zp;
    HEADER:   load = HDR, type = ro;
    CODE:     load = PRG, type = ro,  start = $8000;
    VECTORS:  load = PRG, type = ro,  start = $FFFA;
    IMG:      load = CHR, type = ro;
}
