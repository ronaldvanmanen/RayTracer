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

with AUnit.Test_Caller;

package body Ragnvaldr.Raytracer.Test.Suite is

    package Caller is new AUnit.Test_Caller (Ragnvaldr.Raytracer.Test.Test);

    function Suite return Access_Test_Suite is

        Ret : constant Access_Test_Suite := new Test_Suite;

    begin

        Ret.Add_Test
          (Caller.Create
             ("Test_Intersects_Ray_Outside_And_Points_Towards",
              Test_Intersects_Ray_Outside_And_Points_Towards'Access));

        Ret.Add_Test
          (Caller.Create
             ("Test_Intersects_Ray_Outside_And_Points_Away",
              Test_Intersects_Ray_Outside_And_Points_Away'Access));

        Ret.Add_Test
          (Caller.Create(
           "Test_Intersects_Ray_Inside",
           Test_Intersects_Ray_Inside'Access));

        Ret.Add_Test
          (Caller.Create
             ("Test_Intersects_Ray_Outside_And_Points_Along",
              Test_Intersects_Ray_Outside_And_Points_Along'Access));

        return Ret;
    end Suite;

end Ragnvaldr.Raytracer.Test.Suite;
