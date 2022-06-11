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

end Constraints;
