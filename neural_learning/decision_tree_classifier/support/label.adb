
--  Adapted from scikit-learn/scikit-learn.git sklearn/preprocessing/_label.py

--  A LabelEncoder encodes labels with a value between 0 and n_classes-1 where
--  n is the number of distinct labels. If a label repeats it assigns the same
--  value to as assigned earlier. The categorical values have been converted
--   into numeric values.
--    Examples
--      --------
--      `LabelEncoder` can be used to normalize labels:
--      >>> from sklearn import preprocessing
--      >>> le = preprocessing.LabelEncoder()
--      >>> le.fit([1, 2, 2, 6])
--      LabelEncoder()
--      >>> le.classes_
--      array([1, 2, 6])
--      >>> le.transform([1, 1, 2, 6])
--      array([0, 0, 1, 2]...)
--      >>> le.inverse_transform([0, 0, 1, 2])
--      array([1, 1, 2, 6])

--      It can also be used to transform non-numerical labels (as long as they are
--      hashable and comparable) to numerical labels.
--      >>> le = preprocessing.LabelEncoder()
--      >>> le.fit(["paris", "paris", "tokyo", "amsterdam"])
--      LabelEncoder()
--      >>> list(le.classes_)
--      ['amsterdam', 'paris', 'tokyo']
--      >>> le.transform(["tokyo", "tokyo", "paris"])
--      array([2, 2, 1]...)
--      >>> list(le.inverse_transform([2, 2, 1]))
--      ['tokyo', 'tokyo', 'paris']

with Ada.Assertions; use Ada.Assertions;
--  with Ada.Text_IO; use Ada.Text_IO;

with Classifier_Utilities;
with Encode_Utils;

package body Label is

   type CSR_Matrix (Num_NZ, IP_Length : Positive) is record
      Data    : Integer_Array (1 .. Num_NZ);
      Indices : Integer_Array (1 .. Num_NZ);
      Ind_Ptr : Integer_Array (1 .. IP_Length);
   end record;

   --  -------------------------------------------------------------------------

   function Cum_Sum (A : Boolean_Array) return Integer_Array is
      Result : Integer_Array (A'Range);
      Sum    : Natural := 0;
   begin
      for index in A'Range loop
         if A (index) then
            Sum := Sum + 1;
         end if;
         Result (index) := Sum;
      end loop;

      return Result;

   end Cum_Sum;

   --  -------------------------------------------------------------------------

   procedure Fit (Binarizer : in out Label_Binarizer; Y : Integer_Matrix) is
      --        use Multiclass_Utils;
      Routine_Name : constant String := "Label.Binarizer Fit ";
      --        Label_Type   : Y_Type := Type_Of_Target (Y);
   begin
      Assert (Binarizer.Neg_Label < Binarizer.Pos_Label, Routine_Name &
                "Binarizer.Neg_Label" & Integer'Image (Binarizer.Neg_Label) &
                " must be less than Binarizer.Pos_Label"
              & Integer'Image (Binarizer.Pos_Label));
      Binarizer.Y_Kind := Multiclass_Utils.Type_Of_Target (Y);
      Binarizer.Classes := Multiclass_Utils.Unique_Labels (Y);
   end Fit;

   --  -------------------------------------------------------------------------

   procedure Fit (Encoder : in out Label_Encoder; Y : Integer_Array) is
      Routine_Name : constant String := "Label.Encoder Fit ";
   begin
      Assert (Encoder.Encoder_Kind = Class_Unique, Routine_Name &
                "Label.Fit called with label encoder instead of unique encode");
      Encoder.Uniques := Encode_Utils.Unique (Y);
   end Fit;

   --  -------------------------------------------------------------------------
   --  Fit_Transform fits label encoder and returns encoded labels
   --  Balanced class weights should be given by
   --  n_samples / (n_classes * np.bincount(y))
   function Fit_Transform (Encoder : in out Label_Encoder; Y : Integer_Array)
                            return Natural_Array is
      Encoded_Labels : Natural_Array (1 .. Y'Length);
   begin
      if Encoder.Encoder_Kind = Class_Unique then
         Encoder.Uniques := Encode_Utils.Unique (Y, Encoded_Labels);
      else
         raise Label_Error with
           "Label.Fit_Transform called with label encoder instead of unique encoder";
      end if;

      return Encoded_Labels;

   end Fit_Transform;

   --  -------------------------------------------------------------------------
   --   Inverse_Transform transforms labels back to original encoding
   function Inverse_Transform (Self   : Label_Encoder;
                               Labels : Natural_Array)
                                return Integer_Array is
      use NL_Types;
      aRange  : Integer_Array (1 .. Positive (Self.Uniques'Length));
      Diff    : Natural_List;
      Result  : Integer_Array (1 .. Positive (Labels'Length));
   begin
      for index in aRange'Range loop
         aRange (index) := index;
      end loop;

      Diff := Classifier_Utilities.Set_Diff (Labels, aRange);
      Assert (Diff.Is_Empty,
              "Label.Inverse_Transform Labels vector contains " &
                "previously unseen labels.");

      for index in Result'Range loop
         Result (index) := Self.Uniques (Labels (index));
      end loop;

      return Result;

   end Inverse_Transform;

   --  -------------------------------------------------------------------------

   function Inverse_Transform (Self : Label_Encoder; Y : Integer_Array)
                                return Integer_Array is
      aRange  : Integer_Array (1 .. Positive (Self.Uniques'Length));
      Diff    : NL_Types.Natural_List;
      Result  : Integer_Array (1 .. Positive (Y'Length));
   begin
      for index in Self.Classes'Range loop
         aRange (index) := index;
      end loop;

      Diff := Classifier_Utilities.Set_Diff (Y, aRange);
      Assert (Diff.Is_Empty, "Y contains previously unseen labels.");

      for index in Y'Range loop
         Result (index) := Self.Classes (Y (index));
      end loop;

      return Result;

   end Inverse_Transform;

   --  -------------------------------------------------------------------------

   function Inverse_Transform (Self : Label_Encoder; Y : Integer_Matrix)
                                return Integer_Matrix is
      use NL_Types;
      YT        : constant Integer_Matrix := Transpose (Y);
      aRange    : Integer_Array (1 .. Y'Length);
      Diff      : Natural_List;
      Transform : Integer_Matrix (1 .. YT'Length, 1 .. YT'Length (2));
      YT_Row    : Integer_Array (1 .. YT'Length);
   begin
      for r in Self.Classes'Range loop
         aRange (r) := r;
      end loop;

      for Y_index in YT'Range loop
         for index2 in YT'Range (2) loop
            YT_Row (Y_index) := YT (Y_index, index2);
         end loop;
         Diff := Classifier_Utilities.Set_Diff (YT_Row, aRange);
         Assert (Diff.Is_Empty, "Y contains previously unseen labels.");

         for index2 in YT_Row'Range loop
            Transform (Y_index, index2) := Self.Classes (YT_Row (index2));
         end loop;
      end loop;

      return Transpose (Transform);

   end Inverse_Transform;

   --  -------------------------------------------------------------------------
   --  L416
   function Label_Binarize (Self      : Label_Binarizer; Y : Integer_Array;
                            Classes   : NL_Types.Integer_List;
                            Neg_Label : Integer := 0; Pos_Label : Integer := 1)
                             return Boolean_Matrix is
      use NL_Types;
      use Integer_Sorting;
      Routine_Name   : constant String := "Label.Label_Binarize ";
      Num_Samples    : constant Positive := Y'Length;
      Num_Classes    : constant Positive := Positive (Classes.Length);
      Sorted_Classes : Integer_List := Classes;
      Pos_Switch     : constant Boolean := Pos_Label = 0;
      Pos            : Integer := Pos_Label;
      Y_In_Classes   : Boolean_Array  (Y'Range) := (others => False);
      Y_Seen         : Integer_List;
      Indices        : Integer_List;
      Y_Bin          : Boolean_Matrix (Y'Range, 1 .. Num_Classes);
   begin
      Assert (Neg_Label < Pos_Label, Routine_Name & "Neg_label" &
                Integer'Image (Neg_Label) & " must be less than pos_label" &
                Integer'Image (Pos_Label));
      if Pos_Switch then
         Pos := -Neg_Label;
      end if;

      --  528
      Sort (Sorted_Classes);

      --  L542 y_in_classes = np.in1d(y, classes)
      for index in Y'Range loop
         Y_In_Classes (index) := Sorted_Classes.Contains (Y (index));
      end loop;

      --  L543
      for index in Y'Range loop
         if Y_In_Classes (index) then
            Y_Seen.Append (Y (index));
         end if;
      end loop;

      --  L544
      for index in Sorted_Classes.First_Index ..
        Sorted_Classes.Last_Index loop
         if Y_Seen.Contains (Sorted_Classes (index)) then
            Indices.Append (index);
         end if;
      end loop;

      declare
         --  Data is an array of the non-zero values
         Ind_Length : constant Positive := Positive (Indices.Length);
         Data       : constant Integer_Array (1 .. Ind_Length)
           := (others => Pos);
         Bool_Sum   : constant Integer_Array (1 .. Ind_Length) :=
                        Cum_Sum (Y_In_Classes);
         Ind_Ptr    : Integer_Array (1 .. Ind_Length + 1);
         CSR        : CSR_Matrix (Ind_Length, Ind_Length + 1);
      begin
         CSR.Data := Data;
         CSR.Indices := To_Integer_Array (Indices);
         CSR.Ind_Ptr (1) := 0;
         for index in Bool_Sum'Range loop
            CSR.Ind_Ptr (index + 1) := Bool_Sum  (index);
         end loop;
      end;

      return Y_Bin;

   end Label_Binarize;

   --  -------------------------------------------------------------------------

   function Transform (Self : Label_Binarizer; Y : Integer_Array)
                        return Boolean_Array is
      use Multiclass_Utils;
      Labels : Boolean_Array (Y'Range);
   begin
      if Self.Y_Kind = Y_Multiclass then
         Labels := Label_Binarize (Self, Y, Self.Classes);
      else
         raise Label_Error with
           "Label.Transform called with invalid encoder type.";
      end if;

      return Labels;

   end Transform;

   --  -------------------------------------------------------------------------

   --  Transform returns labels as normalized encodings
   function Transform (Self : Label_Encoder; Y : Integer_Array)
                        return Natural_Array is
      Labels : Natural_Array (1 .. Y'Length);
   begin
      if Self.Encoder_Kind = Class_Unique then
         Labels := Encode_Utils.Encode (Y, Self.Uniques);
      else
         raise Label_Error with
           "Label.Transform called with invalid encoder type.";
      end if;

      return Labels;

   end Transform;

   --  -------------------------------------------------------------------------

end Label;
