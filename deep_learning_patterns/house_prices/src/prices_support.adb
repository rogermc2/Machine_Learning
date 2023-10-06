with Ada.Assertions;        use Ada.Assertions;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

with Maths;

with Basic_Printing; use Basic_Printing;
with Classifier_Loader;
with ML_Types;
with Neural_Loader;

package body Prices_Support is

   package Ordered_Code_Map is new Ada.Containers.Indefinite_Ordered_Maps
     (String, String);
   subtype Code_Map is Ordered_Code_Map.Map;

   Data_Codes : Code_Map;

   NA_Code : constant Integer := 0;

   function Load_Prices
     (File_Name : String; Num_Samples : Positive) return Real_Float_Matrix;
   function Means (M : Real_Float_Matrix) return Real_Float_Vector;
   function Preprocess
     (File_Name : String; Num_Samples : Positive) return Integer_Matrix;
   function Split_Raw_Data
     (Raw_Data : in out ML_Types.Raw_Data_Vector; Num_Features : out Positive)
      return ML_Types.Multi_Output_Data_Record;
   function Standard_Deviation
     (M : Real_Float_Matrix) return Real_Float_Vector;

   --  -------------------------------------------------------------------------

   function Build_Dataset (Train_Length : Positive := 70;
                           Test_Length  : Positive := 30;
                           Num_Features : Natural := 0) return Dataset is
      Routine_Name  : constant String := "Prices_Support.Build_Dataset ";
      Test_Features : constant Integer_Matrix :=
                        Preprocess ("house_prices/test.csv", Test_Length);
      X             : Real_Float_Matrix (Test_Features'Range,
                                         1 .. Test_Features'Length (2) - 1);
      N_Features    : Positive;
--        X_IDs         : Integer_Array  (X'Range);
      X_Means      : Real_Float_Vector (X'Range (2));
      X_SDs        : Real_Float_Vector (X'Range (2));
   begin
      if Num_Features > 0 then
         N_Features := Num_Features;
      else
         N_Features := X'Length (2);
      end if;

      declare
      X_Trimmed  : Real_Float_Matrix (X'Range, 1 .. N_Features);
      theDataset : Dataset (Train_Length, Test_Length, N_Features);
      begin
      for row in X'Range loop
--           X_IDs (row) := Test_Features (row, 1);
         for col in X'Range (2) loop
            X (row, col) := Float (Test_Features (row, col + 1));
         end loop;
      end loop;

      X_Means := Means (X);
      X_SDs   := Standard_Deviation (X);
      Print_Float_Vector (Routine_Name & "X_Means", X_Means, 1, 6);
      Print_Float_Vector (Routine_Name & "X_SDs", X_SDs, 1, 6);

      for row in X'Range loop
            for col in X'Range (2) loop
               X (row, col)  := (X (row, col) - X_Means (col)) / X_SDs (col);
            end loop;
      end loop;

      for row in X_Trimmed'Range loop
         for col in X_Trimmed'Range (2) loop
            X_Trimmed (row, col) := X (row, col + 1);
         end loop;
      end loop;

      theDataset.X_Test := X_Trimmed;
      theDataset.Y_Test :=
        Load_Prices ("house_prices/test_sample_submission.csv",
                     Test_Length);

         return theDataset;
      end;  -- declare block

   end Build_Dataset;

   --  -------------------------------------------------------------------------

   function Load_Prices (File_Name    : String; Num_Samples : Positive)
                         return Real_Float_Matrix is
      use Ada.Containers;
      use ML_Types;
      Routine_Name : constant String := "Prices_Support.Load_Prices ";
      Data_File    : File_Type;
      Raw_CSV_Data : Raw_Data_Vector;
      aRow         : Unbounded_List;
--        IDs          : Integer_Array (1 .. Num_Samples);
      Prices       : Real_Float_Matrix (1 .. Num_Samples, 1 .. 1);
   begin
      Put_Line (Routine_Name & "loading " & File_Name);
      Open (Data_File, In_File, File_Name);
      Raw_CSV_Data := Neural_Loader.Load_Raw_CSV_Data (Data_File, Num_Samples);
      Close (Data_File);

      for row_index in Prices'Range loop
         aRow := Raw_CSV_Data (row_index + 1);
         if aRow.Length > 1 then
            for f_index in 1 .. 2 loop
               declare
                  Row_S     : constant String    := To_String (aRow (f_index));
                  S_Last    : constant Integer   := Row_S'Last;
                  Last_Char : constant Character := Row_S (S_Last);
               begin
                  if Character'Pos (Last_Char) < 32 then
                     aRow (f_index) :=
                       To_Unbounded_String (Row_S (1 .. S_Last - 1));
                  end if;

                  case Neural_Loader.Get_Data_Type (aRow (f_index)) is
                     when Boolean_Type => null;
                     when Float_Type =>
                        Prices (row_index, 1) :=
                          Float'Value (To_String (aRow (f_index)));
                     when Integer_Type => null;
--                          IDs (row_index) :=
--                            Integer'Value (To_String (aRow (f_index)));
                     when UB_String_Type => null;
                  end case;
               end;
            end loop;

         end if;
      end loop;

      return Prices;

   end Load_Prices;

   --  -------------------------------------------------------------------------

   function Means (M : Real_Float_Matrix) return Real_Float_Vector is
      use Real_Float_Arrays;
      M_Length : constant Float := Float (M'Length);
      Sums      : Real_Float_Vector (M'Range (2)) := (others => 0.0);
   begin
      for row in M'Range loop
         for col in M'Range (2) loop
            Sums (row) := Sums (row) + M (row, col);
         end loop;
      end loop;

      return Sums / M_Length;

   end Means;

   --  -------------------------------------------------------------------------

   function Preprocess
     (File_Name : String; Num_Samples : Positive) return Integer_Matrix
   is
      Routine_Name : constant String := "Prices_Support.Preprocess ";
      Data_File    : File_Type;
      Raw_CSV_Data : ML_Types.Raw_Data_Vector;
      Split_Data   : ML_Types.Multi_Output_Data_Record;
      Num_Features : Positive;
   begin
      Put_Line (Routine_Name & "loading " & File_Name);
      Open (Data_File, In_File, File_Name);
      Raw_CSV_Data := Neural_Loader.Load_Raw_CSV_Data (Data_File, Num_Samples);
      Close (Data_File);

      Split_Data := Split_Raw_Data (Raw_CSV_Data, Num_Features);

      declare
         Features : ML_Types.Value_Data_List;
         Result   : Integer_Matrix (1 .. Num_Samples, 1 .. Num_Features);
      begin
         Put_Line (Routine_Name & "proceesing Split_Data");
         for row in Result'Range loop
            Features := Split_Data.Feature_Values (row);
            for col in Result'Range (2) loop
               Result (row, col) := Features (col).Integer_Value;
            end loop;
         end loop;

         return Result;
      end;  --  declare block

   end Preprocess;

   --  -------------------------------------------------------------------------

   function Split_Raw_Data
     (Raw_Data : in out ML_Types.Raw_Data_Vector; Num_Features : out Positive)
      return ML_Types.Multi_Output_Data_Record
   is
      use Ada.Containers;
      use ML_Types;
      Routine_Name   : constant String   := "Prices_Support.Split_Raw_Data ";
      aRow           : Unbounded_List    := Raw_Data.First_Element;
      Features_List  : Value_Data_Lists_2D;
      Feature_Values : Value_Data_List;
      Data           : Multi_Output_Data_Record;
   begin
      Num_Features := Positive (aRow.Length);
      Classifier_Loader.Parse_Header (aRow, Num_Features, Data);

      for row_index in
        Positive'Succ (Raw_Data.First_Index) .. Raw_Data.Last_Index
      loop
         aRow := Raw_Data (row_index);
         if aRow.Length > 1 then
            for f_index in 1 .. Num_Features loop
               declare
                  Row_S     : constant String    := To_String (aRow (f_index));
                  S_Last    : constant Integer   := Row_S'Last;
                  Last_Char : constant Character := Row_S (S_Last);
               begin
                  if Character'Pos (Last_Char) < 32 then
                     aRow (f_index) :=
                       To_Unbounded_String (Row_S (1 .. S_Last - 1));
                  end if;

                  if Neural_Loader.Get_Data_Type (aRow (f_index)) /=
                    Integer_Type
                  then
                     Assert
                       (Data_Codes.Contains (To_String (aRow (f_index))),
                        Routine_Name & To_String (aRow (f_index)) &
                          " is not in Data_Codes map");
                     aRow (f_index) :=
                       To_Unbounded_String
                         (Data_Codes.Element (To_String (aRow (f_index))));
                  end if;

                  Assert
                    (Neural_Loader.Get_Data_Type (aRow (f_index)) =
                       Integer_Type,
                     Routine_Name & To_String (aRow (f_index)) &
                       " is not an integer type");
               end;
            end loop;
            Raw_Data (row_index) := aRow;
         end if;
      end loop;

      Feature_Values.Clear;
      for row_index in
        Positive'Succ (Raw_Data.First_Index) .. Raw_Data.Last_Index
      loop
         aRow := Raw_Data.Element (row_index);  --  Unbound list
         if aRow.Length > 1 then
            Feature_Values.Clear;
            for f_index in 1 .. Num_Features loop
               declare
                  Feat_String : constant String := To_String (aRow (f_index));
                  Value       : Value_Record (Integer_Type);
               begin
                  Value.Integer_Value := Integer'Value (Feat_String);
                  if Value.Integer_Value = NA_Code then
                     Value.Integer_Value := -NA_Code;
                  end if;
                  Feature_Values.Append (Value);
               end;  --  declare block=
            end loop;  --  f_index in 1 .. Num_Features
            Features_List.Append (Feature_Values);
         end if;
      end loop;  --  row_index =

      Data.Feature_Values := Features_List;

      return Data;

   end Split_Raw_Data;

   --  -----------------------------------------------------------------------

   function Standard_Deviation (M : Real_Float_Matrix)
                                return Real_Float_Vector is
      use Maths.Float_Math_Functions;
      M_Length  : constant Float := Float (M'Length);
      Mean_Vals : constant Real_Float_Vector := Means (M);
      Errors_Sq : Real_Float_Matrix (M'Range, M'Range (2));
      Sums      : Real_Float_Vector (M'Range (2)) := (others => 0.0);
      SD        : Real_Float_Vector (M'Range (2));
   begin
      for row in M'Range loop
         for col in M'Range (2) loop
            Errors_Sq (row, col) := (M (row, col) - Mean_Vals (col))**2;
         end loop;
      end loop;

      for row in M'Range loop
         for col in M'Range (2) loop
            Sums (row) := Sums (row) + Errors_Sq (row, col);
         end loop;
      end loop;

      for row in SD'Range loop
         SD (row) := Sqrt (Sums (row) / (M_Length - 1.0));
      end loop;

      return SD;

   end Standard_Deviation;

   --  -------------------------------------------------------------------------
begin
   Data_Codes.Insert ("NA", Integer'Image (NA_Code));
   Data_Codes.Insert ("RH", "1");
   Data_Codes.Insert ("RL", "2");
   Data_Codes.Insert ("TA", "3");
   Data_Codes.Insert ("Y", "4");
   Data_Codes.Insert ("Pave", "5");
   Data_Codes.Insert ("GasA", "6");
   Data_Codes.Insert ("Lvl", "7");
   Data_Codes.Insert ("AllPub", "8");
   Data_Codes.Insert ("Inside", "9");
   Data_Codes.Insert ("Gtl", "10");
   Data_Codes.Insert ("NAmes", "11");
   Data_Codes.Insert ("Feedr", "12");
   Data_Codes.Insert ("Norm", "13");
   Data_Codes.Insert ("1Fam", "14");
   Data_Codes.Insert ("1Story", "15");
   Data_Codes.Insert ("Gable", "16");
   Data_Codes.Insert ("CompShg", "17");
   Data_Codes.Insert ("VinylSd", "18");
   Data_Codes.Insert ("None", "19");
   Data_Codes.Insert ("IR1", "20");
   Data_Codes.Insert ("IR2", "21");
   Data_Codes.Insert ("LwQ", "22");
   Data_Codes.Insert ("Corner", "23");
   Data_Codes.Insert ("Gilbert", "25");
   Data_Codes.Insert ("Normal", "26");
   Data_Codes.Insert ("Hip", "27");
   Data_Codes.Insert ("2Story", "28");
   Data_Codes.Insert ("Ex", "29");
   Data_Codes.Insert ("SBrkr", "30");
   Data_Codes.Insert ("HdBoard", "32");
   Data_Codes.Insert ("Gd", "33");
   Data_Codes.Insert ("No", "34");
   Data_Codes.Insert ("Unf", "35");
   Data_Codes.Insert ("PConc", "36");
   Data_Codes.Insert ("CBlock", "37");
   Data_Codes.Insert ("ALQ", "38");
   Data_Codes.Insert ("Rec", "39");
   Data_Codes.Insert ("Fin", "40");
   Data_Codes.Insert ("MnPrv", "41");
   Data_Codes.Insert ("WD", "42");
   Data_Codes.Insert ("Plywood", "43");
   Data_Codes.Insert ("Attchd", "44");
   Data_Codes.Insert ("TwnhsE", "45");
   Data_Codes.Insert ("MetalSd", "46");
   Data_Codes.Insert ("Fa", "47");
   Data_Codes.Insert ("BrDale", "48");
   Data_Codes.Insert ("Detchd", "49");
   Data_Codes.Insert ("RM", "50");
   Data_Codes.Insert ("Brk", "51");
   Data_Codes.Insert ("NridgHt", "52");
   Data_Codes.Insert ("Stone", "53");
   Data_Codes.Insert ("New", "55");
   Data_Codes.Insert ("Partial", "56");
   Data_Codes.Insert ("CmentBd", "57");
   Data_Codes.Insert ("Typ", "58");
   Data_Codes.Insert ("RFn", "59");
   Data_Codes.Insert ("BrkFBuiltInace", "60");
   Data_Codes.Insert ("Gar2", "61");
   Data_Codes.Insert ("GLQ", "62");
   Data_Codes.Insert ("HLS", "63");
   Data_Codes.Insert ("StoneBr", "64");
   Data_Codes.Insert ("Po", "65");
   Data_Codes.Insert ("FR2", "66");
   Data_Codes.Insert ("BLQ", "67");
   Data_Codes.Insert ("Twnhs", "68");
   Data_Codes.Insert ("COD", "69");
   Data_Codes.Insert ("NPkVill", "70");
   Data_Codes.Insert ("Brk Cmn", "71");
   Data_Codes.Insert ("Wd Sdng", "72");
   Data_Codes.Insert ("PosN", "73");
   Data_Codes.Insert ("CemntBd", "74");
   Data_Codes.Insert ("Mod", "75");
   Data_Codes.Insert ("Mn", "76");
   Data_Codes.Insert ("Blmngtn", "77");
   Data_Codes.Insert ("Av", "78");
   Data_Codes.Insert ("SLvl", "79");
   Data_Codes.Insert ("BuiltIn", "80");
   Data_Codes.Insert ("BrkFace", "81");
   Data_Codes.Insert ("Reg", "82");
   Data_Codes.Insert ("CulDSac", "83");
   Data_Codes.Insert ("NoRidge", "84");
   Data_Codes.Insert ("Somerst", "85");
   Data_Codes.Insert ("FV", "86");
   Data_Codes.Insert ("SawyerW", "87");

end Prices_Support;
