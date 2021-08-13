
with Classifier_Utilities;

package body Label is

   --  -------------------------------------------------------------------------
   --  Values : values to factorize or encode.
   --  Uniques : If not empty, uniques are not determined from passed Values
   --  This can be because the user specified categories or because they
   --  already have been determined in fit.
   --  Encode : if True, encode Values into integer codes based on Uniques.
   --  Check_Unknown : if True check Values for values that are not in Uniques
   --  and raise an error.
   function Encode (Values        : ML_Types.Value_Data_List;
                    Uniques       : in out ML_Types.Value_Data_List;
                    Do_Encode     : Boolean := False;
                    Check_Unknown : Boolean := True) return Natural_List is
      Inverse       : Natural_List;
      Sorted_Values : ML_Types.Value_Data_List := Values;
      Encoded       : Natural_List;
   begin
      Inverse.Clear;
      if Uniques.Is_Empty then
         ML_Types.Value_Data_Sorting.Sort (Sorted_Values);
         Uniques := Classifier_Utilities.Unique_Values (Sorted_Values, Inverse);
      elsif Check_Unknown then
         null;
      end if;

      if Do_Encode then
         for index in Uniques.First_Index .. Uniques.Last_Index loop
            Encoded.Append (index);
         end loop;
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
   --  Balanced class weights should be given by
   --  n_samples / (n_classes * np.bincount(y))
   function Fit_Transform (Self : in out Label_Encoder;
                           Y    : ML_Types.Value_Data_List)
                            return Natural_List is
      Labels  : Natural_List;
      Uniques : ML_Types.Value_Data_List;
   begin
      Uniques.Clear;
      Labels := Encode (Y, Uniques, True);
      Self.Classes := Labels;
      return Labels;
   end Fit_Transform;

   --  -------------------------------------------------------------------------

   --      function Inverse_Transform (Self : in out Label_Encoder;
   --                                  Y    : ML_Types.Value_Data_List)
   --                                 return Natural_List is
   --          Labels  : Natural_List;
   --          Uniques : ML_Types.Value_Data_List;
   --      begin
   --          Labels := Encode (Y, Uniques, Do_Encode => True);
   --          Self.Classes := Labels;
   --          return Labels;
   --      end Inverse_Transform;

   --  -------------------------------------------------------------------------
   --  Balanced class weights should be given by
   --  n_samples / (n_classes * np.bincount(y))
   function Transform (Self : in out Label_Encoder;
                       Y    : ML_Types.Value_Data_List)
                        return Natural_List is
      Labels  : Natural_List;
      Uniques : ML_Types.Value_Data_List;
   begin
      Labels := Encode (Y, Uniques, Do_Encode => True);
      Self.Classes := Labels;
      return Labels;
   end Transform;

   --  -------------------------------------------------------------------------

end Label;
