with Ada.Assertions;        use Ada.Assertions;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

--  with Maths;

--  with Basic_Printing; use Basic_Printing;
with Classifier_Loader;
with ML_Types;
with Neural_Loader;
--  with NL_Types;
--  with Shuffler;
--  with Type_Utilities;

package body Prices_Support is

   package Ordered_Code_Map is new Ada.Containers.Indefinite_Ordered_Maps
     (String, String);
   subtype Code_Map is Ordered_Code_Map.Map;

   Data_Codes : Code_Map;

   NA_Code : constant String := "99999";

   --  function Means (M : Real_Float_Matrix) return Real_Float_Vector;
   function Preprocess
     (File_Name : String; Num_Samples : Positive) return Integer_Matrix;
   function Split_Raw_Data
     (Raw_Data : in out ML_Types.Raw_Data_Vector; Num_Features : out Positive)
      return ML_Types.Multi_Output_Data_Record;
   --  function Standard_Deviation
   --    (M : Real_Float_Matrix) return Real_Float_Vector;

   --  -------------------------------------------------------------------------

   function Build_Dataset return Dataset is
      --  use NL_Types.Float_Package;
      --        use NL_Types.Float_List_Package;
      --        use Type_Utilities;
      Routine_Name  : constant String := "Prices_Support.Build_Dataset ";
      Train_Length  : constant Positive       := 70;
      Test_Length   : constant Positive       := 30;
      --        Train_Data   : constant ML_Types.Multi_Output_Data_Record :=
      --          Classifier_Loader.Load_Data ("house_prices/train.csv", "0,
      --                                       Train_Length);
      --  Prices       : constant ML_Types.Multi_Output_Data_Record :=
      --    Classifier_Loader.Load_Data ("house_prices/sample_submission.csv");
      --        Train_Features : constant NL_Types.Float_List_2D    :=
      --          To_Float_List_2D (Train_Data.Feature_Values);
      Test_Features : constant Integer_Matrix :=
        Preprocess ("house_prices/test.csv", Test_Length);
      --  Target_Item  : NL_Types.Float_List;
      X             : Real_Float_Matrix (Test_Features'Range,
                                         Test_Features'Range (2));
      --  X_Means      : Real_Float_Vector (X'Range (2));
      --  X_SDs        : Real_Float_Vector (X'Range (2));
      --  I0           : ML_Types.Integer_List;
      --  I1           : ML_Types.Integer_List;
      theDataset    : Dataset (Train_Length, Test_Length, 4);
   begin
      Put_Line (Routine_Name);
      for row in X'Range loop
         for col in X'Range (2) loop
            X (row, col) := Float (Test_Features (row, col));
         end loop;
      end loop;

      --  X_Means := Means (X);
      --  X_SDs   := Standard_Deviation (X);
      --  for row in X'RaNeural_Loader.Get_Data_Type (aRow (Positive (f_index)))nge loop
      --     Feature_Row := Features (row);
      --     X (row, 1)  := (X (row, 1) - X_Means (1)) / X_SDs (1);
      --     X (row, 2)  := (X (row, 2) - X_Means (2)) / X_SDs (2);
      --  end loop;

      --  for index in Target.First_Index .. Target.Last_Index loop
      --     Target_Item := Target (index);
      --     if Target_Item.First_Element = 0.0 then
      --        I0.Append (index);
      --     elsif Target_Item.First_Element = 1.0 then
      --        I1.Append (index);
      --     end if;
      --  end loop;

      declare
         --  I0_Length : constant Natural := Integer (I0.Length);
         --  I1_Length : constant Natural := Integer (I1.Length);
         X_Trimmed : Real_Float_Matrix (X'Range, 1 .. 4);
      begin
         --  for row in I0.First_Index .. I0.Last_Index loop
         for row in X_Trimmed'Range loop
            --  X_Trimmed (row + 1, 1) := X (I0 (row), 1);
            --  X_Trimmed (row + 1, 2) := X (I0 (row), 2);
            --  X_Trimmed (row + 1, 3) := X (I0 (row), 3);
            --  X_Trimmed (row + 1, 4) := X (I0 (row), 4);
            for col in X_Trimmed'Range (2) loop
               X_Trimmed (row, col) := X (row, col);
            end loop;
         end loop;

         --  for row in I1.First_Index .. I1.Last_Index loop
         --     X_Trimmed (I0_Length + row + 1, 1) := X (I1 (row), 1);
         --     X_Trimmed (I0_Length + row + 1, 2) := X (I1 (row), 2);
         --     X_Trimmed (I0_Length + row + 1, 3) := X (I1 (row), 3);
         --     X_Trimmed (I0_Length + row + 1, 4) := X (I1 (row), 4);
         --  end loop;

         --  for row in 1 .. 35 loop
         --     theDataset.X_Train (row, 1) := X_Trimmed (row, 1);
         --     theDataset.X_Train (row, 2) := X_Trimmed (row, 2);
         --     theDataset.X_Train (row, 3) := X_Trimmed (row, 3);
         --     theDataset.X_Train (row, 4) := X_Trimmed (row, 4);
         --  end loop;

         --  for row in 36 .. Train_Length loop
         --  for row in theDataset.X_Train'Range loop
         --     for col in theDataset.X_Train'Range (2) loop
         --        theDataset.X_Train (row, col) := X_Trimmed (row, col);
         --     end loop;
         --  end loop;

         --  for index in 1 .. Train_Length loop
         --     if index <= 35 then
         --        theDataset.Y_Train (index, 1) := 0.0;
         --     else
         --        theDataset.Y_Train (index, 1) := 1.0;
         --     end if;
         --  end loop;

         --  for row in 1 .. 15 loop
         --     theDataset.X_Test (row, 1) := X_Trimmed (row + 35, 1);
         --     theDataset.X_Test (row, 2) := X_Trimmed (row + 35, 2);
         --     theDataset.X_Test (row, 3) := X_Trimmed (row + 35, 3);
         --     theDataset.X_Test (row, 4) := X_Trimmed (row + 35, 4);
         --  end loop;

         --  for row in 16 .. Test_Length loop
         --     theDataset.X_Test (row, 1) := X_Trimmed (70 + row, 1);
         --     theDataset.X_Test (row, 2) := X_Trimmed (70 + row, 2);
         --     theDataset.X_Test (row, 3) := X_Trimmed (70 + row, 3);
         --     theDataset.X_Test (row, 4) := X_Trimmed (70 + row, 4);
         for row in theDataset.X_Test'Range loop
            for col in theDataset.X_Test'Range (2) loop
               theDataset.X_Test (row, col) :=
                 X_Trimmed (theDataset.X_Train'Length + 1 + row, col);
            end loop;
         end loop;

         --  for index in 1 .. Test_Length loop
         --     if index <= 15 then
         --        theDataset.Y_Test (index, 1) := 0.0;
         --     else
         --        theDataset.Y_Test (index, 1) := 1.0;
         --     end if;
         --  end loop;
      end;

      --  Shuffler.Shuffle (theDataset.X_Train, theDataset.Y_Train);
      --  Shuffler.Shuffle (theDataset.X_Test, theDataset.Y_Test);

      return theDataset;

   end Build_Dataset;

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
      use Ada.Strings;
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
   Data_Codes.Insert ("NA", NA_Code);
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
