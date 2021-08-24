
--  Adapted from scikit-learn/scikit-learn.git sklearn/preprocessing/_label.py
--  A LabelEncoder encodes labels with a value between 0 and n_classes-1 where
--  n is the number of distinct labels. If a label repeats it assigns the same
--  value to as assigned earlier. The categorical values have been converted into
--  numeric values.

--  with Ada.Text_IO; use Ada.Text_IO;

with Classifier_Utilities;
with Encode_Utils;

package body Label is

   --  -------------------------------------------------------------------------
   --  Fit fits label encoder
   procedure Fit (Encoder : in out Label_Encoder; Y : ML_Types.Value_Data_List) is
   begin
      Encoder.Classes := Encode_Utils.Unique (Y);
   end Fit;

   --  -------------------------------------------------------------------------
   --  Fit_Transform fits label encoder and returns encoded labels
   --  Balanced class weights should be given by
   --  n_samples / (n_classes * np.bincount(y))
   function Fit_Transform (Self : in out Label_Encoder;
                           Y    : ML_Types.Value_Data_List)
                           return Natural_List is
      Encoded_Labels : Natural_List;
   begin
      Self.Classes := Encode_Utils.Unique (Y, Encoded_Labels);
      Classifier_Utilities.Print_Value_List
        ("Label.Fit_Transform Classes", Self.Classes);
      Classifier_Utilities.Print_Natural_List
        ("Label.Fit_Transform Labels", Encoded_Labels);
      return Encoded_Labels;

   end Fit_Transform;

   --  -------------------------------------------------------------------------
   --   Inverse_Transform transforms labels back to original encoding
   function Inverse_Transform (Self : in out Label_Encoder;
                               Y    : Natural_List)
                               return ML_Types.Value_Data_List is
      aRange  : Natural_List := Natural_Package.Empty_Vector;
      Diff    : Natural_List;
      Result  : ML_Types.Value_Data_List :=
                  ML_Types.Value_Data_Package.Empty_Vector;
   begin
      if not Y.Is_Empty then
         for index in 1 .. Positive (Self.Classes.Length) loop
            aRange.Append (index - 1);
         end loop;

         Diff := Classifier_Utilities.Set_Diff (Y, aRange);
         if not Diff.Is_Empty then
            raise Label_Error with
              "Label.Inverse_Transform Y contains previously unseen labels.";
         end if;

         for index in 1 .. Positive (Self.Classes.Length) loop
            Result.Append (Self.Classes.Element (index));
         end loop;
      end if;

      return Result;
   end Inverse_Transform;

   --  -------------------------------------------------------------------------
   --  Transform returns labels as normalized encodings
   function Transform (Self : in out Label_Encoder;
                       Y    : ML_Types.Value_Data_List)
                       return Natural_List is
      Labels  : Natural_List := Natural_Package.Empty_Vector;
   begin
      if not Y.Is_Empty then
         Labels := Encode_Utils.Encode (Y, Self.Classes);
      end if;
      return Labels;
   end Transform;

   --  -------------------------------------------------------------------------

end Label;
