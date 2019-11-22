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

with Ada.Numerics.Real_Arrays; use Ada.Numerics.Real_Arrays;

package Ragnvaldr.Numerics is

    pragma Pure (Numerics);

    subtype Length is Float;

    subtype Axis is Integer range 1 .. 3;

    type Vector is new Real_Vector (Axis);

    type Euler_Vector is record
        Axis : Vector;
        Angle : Float;
    end record;

end Ragnvaldr.Numerics;
