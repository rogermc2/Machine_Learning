
with Classifier_Utilities;

package body Label is

   --  -------------------------------------------------------------------------
   --  Based on _label.py _encode_numpy
   function Encode (Values     : ML_Types.Value_Data_List;
                    Uniques    : in out ML_Types.Value_Data_List;
                    Do_Encode  : Boolean := False) return Integer_List is
      Sorted_Values : ML_Types.Value_Data_List := Values;
      Encoded       : Integer_List;
   begin
      --  if Uniques is not empty, uniques are not determined from passed
      --  values (this can be because the user specified categories or because
      --  they already have been determined in fit).
      if Uniques.Is_Empty then
         ML_Types.Value_Data_Sorting.Sort (Sorted_Values);
         Uniques := Sorted_Values;
      end if;

      if Do_Encode then
         --  encode the values into integer codes based on uniques.
         Encoded := Classifier_Utilities.Search_Sorted_Value_List (Uniques, Values);
      end if;

      return Encoded;
   end Encode;

   --  -------------------------------------------------------------------------
   --  Fit fits label encoder
   function Fit (Y : ML_Types.Value_Data_List) return Label_Encoder is
      Encoder_New : Label_Encoder;
      Uniques     :  ML_Types.Value_Data_List;
   begin
      Encoder_New.Classes := Encode (Y, Uniques);
      return Encoder_New;
   end Fit;

   --  -------------------------------------------------------------------------
   --  Fit_Transform fits label encoder and returns encoded labels
   function Fit_Transform (Self : in out Label_Encoder;
                           Y    : ML_Types.Value_Data_List)
                            return Integer_List is
      Labels   : Integer_List;
      Uniques  : ML_Types.Value_Data_List;
   begin
      Labels := Encode (Y, Uniques, True);
      Self.Classes := Labels;
      return Labels;
   end Fit_Transform;

   --  -------------------------------------------------------------------------

--     function Inverse_Transform (Self : in out Label_Encoder;
--                                 Y    : ML_Types.Value_Data_List)
--                                  return Integer_List is
--        Y_New   : Integer_List;
--        Uniques : ML_Types.Value_Data_List;
--     begin
--        Y_New := Encode (Y, Uniques, Do_Encode => True);
--        Self.Classes := Y_New;
--        return Y_New;
--     end Inverse_Transform;

   --  -------------------------------------------------------------------------

   function Transform (Self : in out Label_Encoder;
                       Y    : ML_Types.Value_Data_List)
                        return Integer_List is
      Labels   : Integer_List;
      Uniques  : ML_Types.Value_Data_List;
   begin
      Labels := Encode (Y, Uniques, Do_Encode => True);
      Self.Classes := Labels;
      return Labels;
   end Transform;

   --  -------------------------------------------------------------------------

end Label;
