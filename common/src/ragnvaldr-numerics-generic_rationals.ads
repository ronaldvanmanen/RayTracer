--  Ragnvaldr Systems -- Raytracer
--
--  Copyright (C) 2020  Ronald van Manen
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

generic
    
    type Integral is range<>;
    
package Ragnvaldr.Numerics.Generic_Rationals is
       
    pragma Pure (Generic_Rationals);
    
    type Rational is record
        Numerator : Integral;
        Denominator : Integral;
    end record;

    function "*" (Left, Right : Rational) return Rational
      with
        Global => null;

    function "+" (Left, Right : Rational) return Rational
      with
        Global => null;
        
    function "-" (Left, Right : Rational) return Rational
      with
        Global => null;

    function Image (Value : Rational) return String
      with
        Global => null;

end Ragnvaldr.Numerics.Generic_Rationals;
