
package body Multiclass_Utils is

   pragma Warnings (Off);

   function Type_Of_Target (Y : Integer_Matrix) return Y_Type is
   begin
      return Y_Continuous;
   end Type_Of_Target;

end Multiclass_Utils;
