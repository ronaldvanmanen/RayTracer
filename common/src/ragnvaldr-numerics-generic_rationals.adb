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

package body Ragnvaldr.Numerics.Generic_Rationals is

    function GreatestCommonDivisor(First : Integral; Second : Integral) return Integral is
        A : Integral := abs First;
        B : Integral := abs Second;
    begin
        loop
            if A = 0 then
                return B;
            end if;
            
            B := B mod A;
            
            if B = 0 then
                return A;
            end if;
                        
            A := A mod B;
        
        end loop;
        
    end GreatestCommonDivisor;
      
    
    function "*" (Left, Right : Rational) return Rational is
        gcd1 : Integral;
        gcd2 : Integral;
        num : Integral;
        den : Integral;
    begin
        gcd1 := GreatestCommonDivisor(left.Numerator, right.Denominator);
        gcd2 := GreatestCommonDivisor(right.Numerator, left.Denominator);
        num := (left.Numerator / gcd1) * (right.Numerator / gcd2);
        den := (left.Denominator / gcd2) * (right.Denominator / gcd1);
        return (Numerator => num, Denominator => den);
    end "*";
      
    function "+" (Left, Right : Rational) return Rational is
        gcd : Integral;
        den : Integral;
        num : Integral;
    begin
        gcd := GreatestCommonDivisor(Left.Denominator, Right.Denominator);
        den := left.Denominator / gcd;
        num := left.Numerator * (right.Denominator / gcd) + right.Numerator * den;

        gcd := GreatestCommonDivisor(num, gcd);
        num := num / gcd;
        den := den * right.Denominator / gcd;

        return (Numerator => num, Denominator => den);
    end "+";
    
    function "-" (Left, Right : Rational) return Rational is
        gcd : Integral;
        den : Integral;
        num : Integral;
    begin
        gcd := GreatestCommonDivisor(left.Denominator, right.Denominator);
        den := left.Denominator / gcd;
        num := left.Numerator * (right.Denominator / gcd) - right.Numerator * den;

        gcd := GreatestCommonDivisor(num, gcd);
        num := num / gcd;
        den := den * right.Denominator / gcd;

        return (Numerator => num, Denominator => den);

    end "-";
    
    function Image (Value : Rational) return String is
    begin
        return 
          Integral'Image (Value.Numerator) & "/"  &
          Integral'Image (Value.Denominator);
    end Image;

end Ragnvaldr.Numerics.Generic_Rationals;
