000100***************************************************************** cpswdx01
000200*                                                               * cpswdx01
000300*  Copyright(C) 1998-2010 Micro Focus. All Rights Reserved.     * cpswdx01
000400*                                                               * cpswdx01
000500***************************************************************** cpswdx01
000600                                                                  cpswdx01
000700***************************************************************** cpswdx01
000800* CPSWDX01.CPY (CICS Version)                                   * cpswdx01
000900*---------------------------------------------------------------* cpswdx01
001000* This copybook is used to provide an common means of calling   * cpswdx01
001100* data module SPSWD01P so that the that module using            * cpswdx01
001200* this copy book is insensitive to it environment.              * cpswdx01
001300* There are different versions for CICS, IMS and INET.          * cpswdx01
001400***************************************************************** cpswdx01
001500* by default use CICS commands to call the module                 cpswdx01
001600     EXEC CICS LINK PROGRAM('SPSWD01P')                           cpswdx01
001700                    COMMAREA(CPSWDD01-DATA)                       cpswdx01
001800                    LENGTH(LENGTH OF CPSWDD01-DATA)               cpswdx01
001900     END-EXEC                                                     cpswdx01
002000*    CALL 'SPSWD01P' USING CPSWDD01-DATA                          cpswdx01
002100                                                                  cpswdx01
002200* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     cpswdx01
