generic
    
    type Real is digits <>;
    
package Ragnvaldr.Numerics.Real_Quaternions is

    type Quaternion is record
        A, B, C, D : Real;
    end record;
    
    function "+" (Left, Right : Quaternion) return Quaternion
      with
        Global => null,
        Post => "+"'Result.A = Left.A + Right.A and 
        "+"'Result.B = Left.B + Right.B and 
        "+"'Result.C = Left.C + Right.C and 
        "+"'Result.D = Left.D + Right.D;
        
    function "-" (Left, Right : Quaternion) return Quaternion
      with
        Global => null;

    function Image (Value : Quaternion) return String
      with
        Global => null;
    
end Ragnvaldr.Numerics.Real_Quaternions;
