with Ada.Assertions;        use Ada.Assertions;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

--  with Basic_Printing; use Basic_Printing;
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
   --  function Means (M : Real_Float_Matrix) return Real_Float_Vector;
   function Preprocess
     (File_Name : String; Num_Samples : Positive) return Integer_Matrix;
   function Split_Raw_Data
     (Raw_Data : in out ML_Types.Raw_Data_Vector; Num_Features : out Positive)
      return ML_Types.Multi_Output_Data_Record;
   --  function Standard_Deviation
   --    (M : Real_Float_Matrix) return Real_Float_Vector;

   --  -------------------------------------------------------------------------

   function Build_Dataset (Train_Length : Positive := 70;
                           Test_Length  : Positive := 30) return Dataset is
      Routine_Name  : constant String := "Prices_Support.Build_Dataset ";
      Num_Features  : constant Positive := 6;
      --        Train_Data   : ML_Types.Multi_Output_Data_Record;
      --  Prices       : constant ML_Types.Multi_Output_Data_Record :=
      --    Classifier_Loader.Load_Data ("house_prices/sample_submission.csv");
      --        Train_Features : constant NL_Types.Float_List_2D;
      Test_Features : constant Integer_Matrix :=
                        Preprocess ("house_prices/test.csv", Test_Length);
      --  Target_Item  : NL_Types.Float_List;
      X             : Real_Float_Matrix (Test_Features'Range,
                                         1 .. Test_Features'Length (2) - 1);
      X_IDs         : Integer_Array  (X'Range);
      --        Y             : Real_Float_Matrix (Test_Features'Range, 1 .. 1) :=
      --                          Load_Prices ("house_prices/test_sample_submission.csv",
      --                                       Test_Length);
      --  X_Means      : Real_Float_Vector (X'Range (2));
      --  X_SDs        : Real_Float_Vector (X'Range (2));
      X_Trimmed     : Real_Float_Matrix (X'Range, 1 .. Num_Features);
      theDataset    : Dataset (Train_Length, Test_Length, Num_Features);
   begin
      Put_Line (Routine_Name);
      for row in X'Range loop
         X_IDs (row) := Test_Features (row, 1);
         for col in X'Range (2) loop
            X (row, col) := Float (Test_Features (row, col + 1));
         end loop;
      end loop;
--        Print_Integer_Array (Routine_Name & "X_IDs", X_IDs);

      --  X_Means := Means (X);
      --  X_SDs   := Standard_Deviation (X);
      --  for row in X'Range loop
      --     Feature_Row := Features (row);
      --     X (row, 1)  := (X (row, 1) - X_Means (1)) / X_SDs (1);
      --     X (row, 2)  := (X (row, 2) - X_Means (2)) / X_SDs (2);
      --  end loop;

      for row in X_Trimmed'Range loop
         for col in X_Trimmed'Range (2) loop
            X_Trimmed (row, col) := X (row, col + 1);
         end loop;
      end loop;

      theDataset.X_Test := X_Trimmed;
      theDataset.Y_Test :=
        Load_Prices ("house_prices/test_sample_submission.csv",
                     Test_Length);
--        Print_Float_Matrix (Routine_Name & "Y_Test", theDataset.Y_Test);

      return theDataset;

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
      IDs          : Integer_Array (1 .. Num_Samples);
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
                        IDs (row_index) :=
                          Integer'Value (To_String (aRow (f_index)));
                     when UB_String_Type => null;
                  end case;
               end;
            end loop;

         end if;
      end loop;

      Put_Line (Routine_Name & "done");
      return Prices;

   end Load_Prices;

   --  -------------------------------------------------------------------------

   --  function Means (M : Real_Float_Matrix) return Real_Float_Vector is
   --     M_Length : constant Float := Float (M'Length);
   --     Sum1     : Float          := 0.0;
   --     Sum2     : Float          := 0.0;
   --     Sum3     : Float          := 0.0;
   --     Sum4     : Float          := 0.0;
   --     Result   : Real_Float_Vector (M'Range (2));
   --  begin
   --     for row in M'Range loop
   --        Sum1 := Sum1 + M (row, 1);
   --        Sum2 := Sum2 + M (row, 2);
   --        Sum3 := Sum3 + M (row, 3);
   --        Sum4 := Sum4 + M (row, 4);
   --     end loop;
   --
   --     for row in M'Range loop
   --        Result (1) := Sum1 / M_Length;
   --        Result (2) := Sum2 / M_Length;
   --        Result (3) := Sum3 / M_Length;
   --        Result (4) := Sum4 / M_Length;
   --     end loop;
   --
   --     return Result;
   --
   --  end Means;

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
      Put_Line (Routine_Name & "Raw_Data split");
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

         Put_Line (Routine_Name & "done");
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
                     --  Put_Line (Routine_Name & To_String (aRow (f_index)) &
                     --              " is not an integer type");
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

      Put_Line (Routine_Name & "done");
      return Data;

   end Split_Raw_Data;

   --  -----------------------------------------------------------------------

   --  function Standard_Deviation (M : Real_Float_Matrix) return Real_Float_Vector
   --  is
   --     use Maths.Float_Math_Functions;
   --     M_Length  : constant Float             := Float (M'Length);
   --     Mean_Vals : constant Real_Float_Vector := Means (M);
   --     Errors_Sq : Real_Float_Matrix (M'Range, M'Range (2));
   --     Sum1      : Float                      := 0.0;
   --     Sum2      : Float                      := 0.0;
   --     Sum3      : Float                      := 0.0;
   --     Sum4      : Float                      := 0.0;
   --     SD        : Real_Float_Vector (M'Range (2));
   --  begin
   --     for row in M'Range loop
   --        Errors_Sq (row, 1) := (M (row, 1) - Mean_Vals (1))**2;
   --        Errors_Sq (row, 2) := (M (row, 2) - Mean_Vals (2))**2;
   --        Errors_Sq (row, 3) := (M (row, 3) - Mean_Vals (3))**2;
   --        Errors_Sq (row, 4) := (M (row, 4) - Mean_Vals (4))**2;
   --     end loop;
   --
   --     for row in M'Range loop
   --        Sum1 := Sum1 + Errors_Sq (row, 1);
   --        Sum2 := Sum2 + Errors_Sq (row, 2);
   --        Sum3 := Sum3 + Errors_Sq (row, 3);
   --        Sum4 := Sum4 + Errors_Sq (row, 4);
   --     end loop;
   --
   --     SD (1) := Sqrt (Sum1 / (M_Length - 1.0));
   --     SD (2) := Sqrt (Sum2 / (M_Length - 1.0));
   --     SD (3) := Sqrt (Sum3 / (M_Length - 1.0));
   --     SD (4) := Sqrt (Sum4 / (M_Length - 1.0));
   --
   --     return SD;
   --
   --  end Standard_Deviation;

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
