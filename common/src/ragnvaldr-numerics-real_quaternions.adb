package body Ragnvaldr.Numerics.Real_Quaternions is

    function "+" (Left, Right : Quaternion) return Quaternion is
    begin
        return (A => Left.A + Right.A, 
                B => Left.B + Right.B, 
                C => Left.C + Right.C, 
                D => Left.D + Right.D);
    end "+";
    
    function "-" (Left, Right : Quaternion) return Quaternion is
    begin
        return (A => Left.A - Right.A, 
                B => Left.B - Right.B, 
                C => Left.C - Right.C, 
                D => Left.D - Right.D);
    end "-";
    
    function Image (Value : Quaternion) return String is
    begin
        return 
          Real'Image (Value.A) & "+"  &
          Real'Image (Value.B) & "i+" &
          Real'Image (Value.C) & "j+" &
          Real'Image (Value.D) & "k";
    end Image;

end Ragnvaldr.Numerics.Real_Quaternions;
