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

with AUnit.Test_Fixtures;

package Ragnvaldr.Raytracer.Test
  with
    SPARK_Mode => Off
  is

    type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

    procedure Test_Intersects (T : in out Test);

    procedure Test_Not_Intersects (T : in out Test);

end Ragnvaldr.Raytracer.Test;
