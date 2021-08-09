
package body Label is

   package Integer_List_Sorter is new Integer_Package.Generic_Sorting;

   --  -------------------------------------------------------------------------

   function Encode (Values     : ML_Types.Value_Data_List;
                    Uniques    : Integer_List :=
                      Integer_Package.Empty_Vector;
                    Do_Encode  : Boolean := False) return Integer_List is
      New_Uniques : Integer_List := Uniques;
   begin
      if Uniques.Is_Empty then
         Integer_List_Sorter.Sort (New_Uniques);
      elsif Do_Encode then
         null;  --  Already "encode" as Uniques is avector
      end if;
      return New_Uniques;
   end Encode;

   --  -------------------------------------------------------------------------
   --  Fit fits label encoder
   function Fit (Self : Label_Encoder; Y : ML_Types.Value_Data_List)
                 return Label_Encoder is
      Encoder_New : Label_Encoder;
   begin
      Encoder_New.Classes := Encode (Y);
      return Encoder_New;
   end Fit;

   --  -------------------------------------------------------------------------
   --  Fit_Transform fits label encoder and returns encoded labels
   function Fit_Transform (Self : in out Label_Encoder;
                           Y    : Integer_List := Integer_Package.Empty_Vector)
                           return Integer_List is
      Y_New : Integer_List := Y;
   begin
      Y_New := Encode (Y, Do_Encode => True);
      Self.Classes := Y_New;
      return Y_New;
   end Fit_Transform;

   --  -------------------------------------------------------------------------

   function Inverse_Transform (Self : in out Label_Encoder;
                               Y    : Integer_List := Integer_Package.Empty_Vector)
                               return Integer_List is
      Y_New : Integer_List := Y;
   begin
      Y_New := Encode (Y, Do_Encode => True);
      Self.Classes := Y_New;
      return Y_New;
   end Inverse_Transform;

   --  -------------------------------------------------------------------------

   function Transform (Self : in out Label_Encoder;
                       Y    : Integer_List := Integer_Package.Empty_Vector)
                       return Integer_List is
      Y_New : Integer_List := Y;
   begin
      Y_New := Encode (Y, Do_Encode => True);
      Self.Classes := Y_New;
      return Y_New;
   end Transform;

   --  -------------------------------------------------------------------------

end Label;
