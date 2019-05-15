/* -*- js2 -*-*/

/*@license
 *
 * ./servers/templates/mobile.warning.js is part of Tootsville
 *
 * Copyright   © 2008-2017   Bruce-Robert  Pocock;   ©  2018,2019   The
 * Corporation for Inter-World Tourism and Adventuring (ciwta.org).
 *
 * This program is Free Software:  you can redistribute it and/or modify
 * it  under the  terms  of the  GNU Affero  General  Public License  as
 * published by  the Free Software  Foundation; either version 3  of the
 * License, or (at your option) any later version.
 *
 * This program is  distributed in the hope that it  will be useful, but
 * WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
 * MERCHANTABILITY  or FITNESS  FOR A  PARTICULAR PURPOSE.  See the  GNU
 * Affero General Public License for more details.
 *
 * You should  have received  a copy  of the  GNU Affero  General Public
 * License     along    with     this    program.     If    not,     see
 * <https://www.gnu.org/licenses/>.
 *
 * You can reach CIWTA at https://ciwta.org/, or write to us at:
 *
 * PO Box 23095
 *
 * Oakland Park, FL 33307-3095
 *
 * USA
 *
 */

if (platform.os.family == 'Android' ||
       platform.os.family == 'iOS' ||
       platform.os.family.indexOf('Phone') >= 0) {
     alert('WARNING: This is an online game. Using it over a \n' +
           'metered connection (like most mobile/cellular services) \n' +
           'may use up A LOT of data. Please make sure that you are \n' +
           'connected to a WiFi or wired network when playing if \n' +
           'your carrier charges extra for data usage.');
 }
