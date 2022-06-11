--  Based on scipy/optimise/_constraints.py

package body Constraints is

   procedure Get_Bounds (Bounds       : Bounds_List;
                         Lower, Upper : out NL_Types.Float_List) is
   begin
      for index in Bounds.First_Index .. Bounds.Last_Index loop
         Lower.Append (Bounds.Element (index).Lower);
         Upper.Append (Bounds.Element (index).Upper);
      end loop;

   end Get_Bounds;

   --  ------------------------------------------------------------------------

   function Get_Lower (Bounds : Bounds_List) return Real_Float_Vector is
     Lower : Real_Float_Vector (1 .. Positive (Bounds.Length));
   begin
      for index in Bounds.First_Index .. Bounds.Last_Index loop
         Lower (index) := (Bounds.Element (index).Lower);
      end loop;

      return Lower;

   end Get_Lower;

   --  ------------------------------------------------------------------------

   function Get_Upper (Bounds : Bounds_List) return Real_Float_Vector is
     Upper : Real_Float_Vector (1 .. Positive (Bounds.Length));
   begin
      for index in Bounds.First_Index .. Bounds.Last_Index loop
         Upper (index) := (Bounds.Element (index).Lower);
      end loop;

      return Upper;

   end Get_Upper;

   --  ------------------------------------------------------------------------

end Constraints;
