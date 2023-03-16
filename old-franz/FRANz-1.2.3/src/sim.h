#ifndef SIM_H
#define SIM_H
/*
 * $Id: sim.h 1366 2009-03-09 16:07:38Z markus $
 *
 * Copyright (C) 2008-2009 Universitaet Leipzig  
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * Markus Riester, markus@bioinf.uni-leipzig.de */


typedef struct S_SIMULATION *SIMULATION;

SIMULATION SIMstart(void);
void SIMdump(FILE *, SIMULATION);
void SIMdumpDetailed(FILE *, SIMULATION);
void SIMdestroy(SIMULATION);

/* 
  vim: ft=c sw=4 ts=4 expandtab
*/

#endif
