
--  Adapted from scikit-learn/scikit-learn.git sklearn/preprocessing/_label.py

with Ada.Text_IO; use Ada.Text_IO;

with Classifier_Utilities;
with Encode_Utils;

package body Label is

   function Encode_Check_Unknown
     (Values : ML_Types.Value_Data_List; Uniques : ML_Types.Value_Data_List)
      return ML_Types.Value_Data_List;

   --  -------------------------------------------------------------------------
   --  Values : values to factorize or encode.
   --  Uniques : If not empty, uniques are not determined from passed Values
   --  This can be because the user specified categories or because they
   --  already have been determined in fit.
   --  Encode : if True, encode Values into integer codes based on Uniques.
   --  Check_Unknown : if True check Values for values that are not in Uniques
   --  and raise an error.
   function Encode (Values        : ML_Types.Value_Data_List;
                    Encoded       : out Natural_List;
                    Do_Encode     : Boolean := False;
                    Uniques_In    : ML_Types.Value_Data_List :=
                      ML_Types.Value_Data_Package.Empty_Vector;
                    Check_Unknown : Boolean := True)
                    return ML_Types.Value_Data_List is

      Uniques       : ML_Types.Value_Data_List;
      Inverse       : Natural_List :=
                        Classifier_Types.Natural_Package.Empty_Vector;
      Sorted_Values : ML_Types.Value_Data_List := Values;
      Diff          : ML_Types.Value_Data_List;
   begin
      Encoded := Classifier_Types.Natural_Package.Empty_Vector;
      ML_Types.Value_Data_Sorting.Sort (Sorted_Values);
      if Uniques_In.Is_Empty then
         if Do_Encode then
            Uniques := Encode_Utils.Unique
              (Sorted_Values, Inverse, Return_Inverse => True);
            for index in Inverse.First_Index .. Inverse.Last_Index loop
               Encoded.Append (Inverse.Element (index));
            end loop;
         else
            Uniques :=
              Encode_Utils.Unique (Sorted_Values, Inverse);
         end if;

      elsif Do_Encode then
         if Check_Unknown then
            Put_Line ("Label.Encode Check_Unknown.");
            Diff := Encode_Check_Unknown (Values, Uniques_In);
            if not Diff.Is_Empty then
               Classifier_Utilities.Print_Value_List
                 ("Unique list", Uniques_In);
               Classifier_Utilities.Print_Value_List
                 ("Unseen labels", Diff);
               raise Label_Error with
                 "Label.Encode Values contains previously unseen labels.";
            end if;
         end if;

         for index in Uniques.First_Index .. Uniques.Last_Index loop
            Encoded.Append (index);
         end loop;
      end if;

      Classifier_Utilities.Print_Value_List
        ("Label.Encode Uniques", Uniques);
      Classifier_Utilities.Print_Natural_List
        ("Label.Encode Encoded", Encoded);

      return Uniques;
   end Encode;

   --  -------------------------------------------------------------------------

   function Encode_Check_Unknown
     (Values : ML_Types.Value_Data_List; Uniques : ML_Types.Value_Data_List)
      return ML_Types.Value_Data_List is
      use ML_Types;
      No_Inverse  : Natural_List :=
                      Classifier_Types.Natural_Package.Empty_Vector;
      Unique_Vals : constant Value_Data_List :=
                      Encode_Utils.Unique (Values, No_Inverse);
      aVal        : Value_Record;
      Diff        : Value_Data_List;
   begin
      for index in Unique_Vals.First_Index .. Unique_Vals.Last_Index loop
         aVal := Unique_Vals.Element (index);
         if not Uniques.Contains (aVal) then
            Diff.Append (aVal);
         end if;
      end loop;

      return Diff;
   end Encode_Check_Unknown;

   --  -------------------------------------------------------------------------
   --  A Fit function adjusts weights according to data values so that better accuracy can be achieved.
   --  Fit fits label encoder
   function Fit (Y : ML_Types.Value_Data_List) return Label_Encoder is
      Encoder_New : Label_Encoder;
      Y_Inverse   :  Natural_List :=
                        Natural_Package.Empty_Vector;
--        Uniques     :  ML_Types.Value_Data_List;
   begin
--        pragma Warnings (Off, Uniques);
--        Encoder_New.Classes := Encode (Y, Encoder_New.Classes);
        Encoder_New.Classes := Encode_Utils.Unique (Y, Y_Inverse);
      return Encoder_New;
   end Fit;

   --  -------------------------------------------------------------------------
   --  Fit_Transform fits label encoder and returns encoded labels
   --  Balanced class weights should be given by
   --  n_samples / (n_classes * np.bincount(y))
   function Fit_Transform (Self : in out Label_Encoder;
                           Y    : ML_Types.Value_Data_List)
                           return ML_Types.Value_Data_List is
      Uniques  : ML_Types.Value_Data_List :=
                   ML_Types.Value_Data_Package.Empty_Vector;
      Labels   : Natural_List;
   begin
      Uniques := Encode (Y, Labels, True);
      Classifier_Utilities.Print_Value_List
        ("Label.Fit_Transform Uniques", Uniques);
      Classifier_Utilities.Print_Natural_List
        ("Label.Fit_Transform Labels", Labels);
      Self.Classes := Labels;
      return Uniques;
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
      pragma Warnings (Off, Uniques);
      Uniques := Encode (Y, Labels, Do_Encode => True);
      Self.Classes := Labels;
      return Labels;
   end Transform;

   --  -------------------------------------------------------------------------

end Label;
