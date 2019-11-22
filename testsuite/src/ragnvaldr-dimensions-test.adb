--  Ragnvaldr Systems -- Raytracer
--
--  Copyright (C) 2019  Ronald van Manen
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <https://www.gnu.org/licenses/>.

with AUnit.Assertions; use AUnit.Assertions;
with Ada.Exceptions;
with Ragnvaldr.Dimensions;
  
package body Ragnvaldr.Dimensions.Test is

    procedure Test_Free_Fall_Problem (T : in out Test) is
        
        pragma Unreferenced(T);
        
        Gravity           : constant Acceleration := 9.81 * Meter / (Second ** 2);
        Elapsed_Time      : Time := 10.0 * Second;
        Actual_Distance   : Length := 0.5 * Gravity * Elapsed_Time ** 2;
        
    begin -- Test_Free_Fall_Problem
        Assert
          (
           Actual_Distance in 490.5000 * Meter .. 490.5001  * Meter, 
           "The expected distance traveled in 10 s of free fall is 490.50 m"
          );
    
    end Test_Free_Fall_Problem;

end Ragnvaldr.Dimensions.Test;
