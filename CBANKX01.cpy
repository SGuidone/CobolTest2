000100***************************************************************** cbankx01
000200*                                                               * cbankx01
000300*  Copyright(C) 1998-2010 Micro Focus. All Rights Reserved.     * cbankx01
000400*                                                               * cbankx01
000500***************************************************************** cbankx01
000600                                                                  cbankx01
000700***************************************************************** cbankx01
000800* CBANKX01.CPY (CICS Version)                                   * cbankx01
000900*---------------------------------------------------------------* cbankx01
001000* This copybook is used to provide an common means of calling   * cbankx01
001100* data access module DBANK01P so that the that module using     * cbankx01
001200* this copy book is insensitive to it environment.              * cbankx01
001300* There are different versions for CICS, IMS and INET.          * cbankx01
001400***************************************************************** cbankx01
001500* by default use CICS commands to call the module                 cbankx01
001600     EXEC CICS LINK PROGRAM('DBANK01P')                           cbankx01
001700                    COMMAREA(CD01-DATA)                           cbankx01
001800                    LENGTH(LENGTH OF CD01-DATA)                   cbankx01
001900     END-EXEC                                                     cbankx01
002000*    CALL 'DBANK01P' USING CD01-DATA                              cbankx01
002100                                                                  cbankx01
002200* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     cbankx01
