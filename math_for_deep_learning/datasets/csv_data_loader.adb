
with Ada.Assertions; use Ada.Assertions;
with Ada.Directories;
with Ada.Streams.Stream_IO;
with Ada.Text_IO; use Ada.Text_IO;

with Data_Splitter;
with Load_Dataset;
with Neural_Processes;
with Shuffler;

package body CSV_Data_Loader is

   procedure Save_State (Dataset_Name : String; State : Base_State);

   --  -------------------------------------------------------------------------

--     function Get_State
--       (Dataset               : Load_Dataset.Digits_Data_Record;
--        Train_Size, Test_Size : Positive; Shuffle : Boolean := True)
--        return Base_State is
--        Routine_Name : constant String := "CSV_Data_Loader.Get_State ";
--        Num_Features : constant Positive := Dataset.Num_Features;
--        X            : Real_Float_Matrix :=
--                         To_Real_Float_Matrix (Dataset.Features);
--        Y            : Integer_Matrix := To_Integer_Matrix (Dataset.Target);
--        Train_X      : Real_Float_Matrix (1 .. Train_Size, 1 .. Num_Features);
--        Train_Y      : Integer_Matrix (1 .. Train_Size, 1 .. 1);
--        Test_X       : Real_Float_Matrix (1 .. Test_Size, 1 .. Num_Features);
--        Test_Y       : Integer_Matrix (1 .. Test_Size, 1 .. 1);
--        Data         : Base_State (Train_Size, Test_Size, Num_Features);
--     begin
--        Assert (Y'Length = X'Length, Routine_Name &
--                  "Y length" & Integer'Image (Y'Length) &
--                  " is different to X length" &
--                  Natural'Image (Positive (X'Length)));
--
--        if Shuffle then
--           Put_Line (Routine_Name & "shuffling");
--           Shuffler.Shuffle (X, Y);
--        end if;
--        --        Printing.Print_Float_List ("permuted features row 16", X.Element (16));
--        Put_Line (Routine_Name & "splitting data");
--        Data_Splitter.Train_Test_Split
--          (X => X, Y => Y, Train_Size => Train_Size, Test_Size => Test_Size,
--           Train_X => Train_X, Train_Y => Train_Y,
--           Test_X => Test_X, Test_Y => Test_Y);
--
--        Data.Train_X := Train_X;
--        Data.Train_Y := Train_Y;
--        Data.Test_X := Test_X;
--        for index in Test_Y'Range loop
--           Data.Test_Y (index) := Float (Test_Y (index, 1));
--        end loop;
--
--        return Data;
--
--     end Get_State;

   --  -------------------------------------------------------------------------

   function Get_State
     (Dataset_Name : String; Train_Size, Test_Size : Positive;
      Shuffle      : Boolean := True) return Base_State is
      use Ada.Directories;
      use Ada.Streams;
      use Stream_IO;
      Routine_Name   : constant String := "CSV_Data_Loader.Get_State ";
      State_File     : constant String := Dataset_Name & ".sta";
      Has_Data       : constant Boolean := Exists (State_File);
      Num_Features   : constant Positive := 196;
      File_ID        : Stream_IO.File_Type;
      aStream        : Stream_Access;
   begin
      if Has_Data then
         Put_Line (Routine_Name & "restoring state");
         Open (File_ID, In_File, State_File);
         aStream := Stream (File_ID);

         declare
            Data : Base_State (Train_Size, Test_Size, Num_Features);
         begin
            Base_State'Read (aStream, Data);
            Close (File_ID);
            Put_Line (Routine_Name & "state restored");
            return Data;
         end;

      else
         Put_Line (Routine_Name & "Fetch data");
         declare
            use Real_Float_Arrays;
            Data_Record  : constant Load_Dataset.Digits_Data_Record :=
                            Neural_Processes.Load_Data_Set
                              (Dataset_Name & ".csv");
            X            : Real_Float_Matrix := To_Real_Float_Matrix
              (Data_Record.Features) / 255.0;
            Y            : Integer_Array := Data_Record.Target;
            Num_Features : constant Positive := Data_Record.Num_Features;
            Train_X      : Real_Float_Matrix (1 .. Train_Size,
                                              1 .. Num_Features);
            Train_Y      : Integer_Array (1 .. Train_Size);
            Train_Y2     : Real_Float_Matrix (1 .. Train_Size, 1 .. 1);
            Test_X       : Real_Float_Matrix (1 .. Test_Size,
                                              1 .. Num_Features);
            Test_Y       : Integer_Array (1 .. Test_Size);
            Test_Y2      : Integer_Matrix (1 .. Test_Size, 1 .. 1);
            Data         : Base_State (Train_Size, Test_Size, Num_Features);
         begin
            Put_Line (Routine_Name & "csv loaded");
            Assert (Y'Length = X'Length, Routine_Name &
                      "Y length" & Integer'Image (Y'Length) &
                      " is different to X length" &
                      Natural'Image (Positive (X'Length)));

            if Shuffle then
               Put_Line (Routine_Name & "shuffling");
               Shuffler.Shuffle (X, Y);
            end if;

            Put_Line (Routine_Name & "splitting data");
            Data_Splitter.Train_Test_Split
              (X => X, Y => Y, Train_Size => Train_Size, Test_Size => Test_Size,
               Train_X => Train_X, Train_Y => Train_Y,
               Test_X => Test_X, Test_Y => Test_Y);

            for row in Train_Y2'First .. Train_Y2'Last loop
               Train_Y2 (row, 1) := Float (Train_Y (row));
            end loop;

            for row in Test_Y2'First .. Test_Y2'Last loop
               Test_Y2 (row, 1) := Test_Y (row);
            end loop;

            Data.Train_X := Train_X;
            Data.Train_Y := Train_Y2;
            Data.Test_X := Test_X;
            for index in Test_Y2'Range loop
               Data.Test_Y (index) := Float (Test_Y2 (index, 1));
            end loop;

            Save_State (Dataset_Name, Data);
            return Data;
         end;
      end if;

   end Get_State;

   --  -------------------------------------------------------------------------

   procedure Save_State (Dataset_Name : String; State : Base_State) is
      use Ada.Streams;
      use Stream_IO;
      --        Routine_Name : constant String := "CSV_Data_Loader.Save_State ";
      State_File   : constant String := Dataset_Name & ".sta";
      File_ID      : Stream_IO.File_Type;
      aStream      : Stream_Access;
   begin
      Create (File_ID, Out_File, State_File);
      aStream := Stream (File_ID);
      Base_State'Write (aStream, State);
      Close (File_ID);
--        pragma Unreferenced (File_ID);

   end Save_State;

   --  -------------------------------------------------------------------------

end CSV_Data_Loader;
