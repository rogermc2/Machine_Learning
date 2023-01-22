
with Ada.Assertions; use Ada.Assertions;
with Ada.Directories;
with Ada.Streams.Stream_IO;
with Ada.Text_IO; use Ada.Text_IO;

with Basic_Printing; use Basic_Printing;
with Load_Dataset;
with Neural_Processes;
with Shuffler;

package body CSV_Data_Loader is

   procedure Save_State (Dataset_Name : String; State : Base_Split_State);

   procedure Train_Test_Split
     (X           : Real_Float_Matrix; Y : Integer_Array;
      Train_Size  : Natural; Test_Size  : Natural;
      Train_X     : out Real_Float_Matrix; Train_Y : out Integer_Array;
      Test_X      : out Real_Float_Matrix; Test_Y : out Integer_Array);

   --  -------------------------------------------------------------------------

   function Categorize (Labels : Integer_Array) return Binary_Matrix is
      Result : Binary_Matrix (Labels'Range, 0 .. 9) :=
                 (others => (others => 0));
   begin
      for row in Labels'Range loop
         Result (row, Labels (row)) := 1;
      end loop;
      return Result;

   end Categorize;

   --  -------------------------------------------------------------------------

   function Get_Split_State
     (Dataset_Name : String; Train_Size, Test_Size : Positive;
      Shuffle      : Boolean := True; Reload : Boolean := False)
      return Base_Split_State is
      use Ada.Directories;
      use Ada.Streams;
      use Stream_IO;
      Routine_Name   : constant String := "CSV_Data_Loader.Get_Split_State ";
      State_File     : constant String := Dataset_Name & ".sta";
      Has_Data       : constant Boolean := Exists (State_File);
      Num_Features   : constant Positive := 784;
      File_ID        : Stream_IO.File_Type;
      aStream        : Stream_Access;
   begin
      if Has_Data and not Reload then
         Put_Line (Routine_Name & "restoring state");
         Open (File_ID, In_File, State_File);
         aStream := Stream (File_ID);

         declare
            Data : Base_Split_State (Train_Size, Test_Size, Num_Features);
         begin
            Base_Split_State'Read (aStream, Data);
            Close (File_ID);
            Put_Line (Routine_Name & "state restored");
            return Data;
         end;

      else
         Put_Line (Routine_Name & "fetching data");
         declare
            use Real_Float_Arrays;
            Data_Record  : constant Load_Dataset.Digits_Data_Record :=
                             Neural_Processes.Load_Data_Set
                               (Dataset_Name & ".csv");
            X            : Real_Float_Matrix := To_Real_Float_Matrix
              (Data_Record.Features) / 255.0;
            Y            : Integer_Array := Data_Record.Target;
            Train_Y      : Integer_Array (1 .. Train_Size);
            Test_Y       : Integer_Array (1 .. Test_Size);
            Data         : Base_Split_State (Train_Size, Test_Size,
                                             Num_Features);
         begin
            Put_Line (Routine_Name & "csv loaded");
            Print_Matrix_Dimensions (Routine_Name & "X" , X);
            New_Line;
            Assert (Y'Length = X'Length, Routine_Name &
                      "Y length" & Integer'Image (Y'Length) &
                      " is different to X length" &
                      Natural'Image (Positive (X'Length)));

            if Shuffle then
               Put_Line (Routine_Name & "shuffling");
               Shuffler.Shuffle (X, Y);
            end if;

            Put_Line (Routine_Name & "splitting data");
            Train_Test_Split
              (X => X, Y => Y, Train_Size => Train_Size, Test_Size => Test_Size,
               Train_X => Data.Train_X , Train_Y => Train_Y
,
               Test_X => Data.Test_X, Test_Y => Test_Y);

            Data.Train_Y := Categorize (Train_Y);

            for index in Test_Y'Range loop
               Data.Test_Y (index) := Float (Test_Y (index));
            end loop;

            Save_State (Dataset_Name, Data);
            return Data;
         end;
      end if;

   end Get_Split_State;

   --  -------------------------------------------------------------------------

   procedure Save_State (Dataset_Name : String; State : Base_Split_State) is
      use Ada.Streams;
      use Stream_IO;
      --        Routine_Name : constant String := "CSV_Data_Loader.Save_State ";
      State_File   : constant String := Dataset_Name & ".sta";
      File_ID      : Stream_IO.File_Type;
      aStream      : Stream_Access;
   begin
      Create (File_ID, Out_File, State_File);
      aStream := Stream (File_ID);
      Base_Split_State'Write (aStream, State);
      Close (File_ID);

   end Save_State;

   --  -------------------------------------------------------------------------

   procedure Train_Test_Split
     (X          : Real_Float_Matrix; Y : Integer_Array;
      Train_Size : Natural; Test_Size : Natural;
      Train_X    : out Real_Float_Matrix; Train_Y : out Integer_Array;
      Test_X     : out Real_Float_Matrix; Test_Y : out Integer_Array) is
      Routine_Name : constant String := "CSV_Data_Loader.Train_Test_Split ";
      Num_Samples  : constant Positive := X'Length;
   begin
      Assert (Natural (Y'Length) = Num_Samples, Routine_Name &
                "Y length" & Integer'Image (Integer (Y'Length)) &
                " is different to X length" & Natural'Image (Num_Samples));

      for row in 1 .. Train_Size loop
         for col in X'Range (2) loop
            Train_X (row, col) := X (row, col);
         end loop;
         Train_Y (row) := Y (row);
      end loop;

      for row in 1 .. Test_Size loop
         for col in X'Range (2) loop
            Test_X (row, col) := X (row + Train_Size, col);
         end loop;
         Test_Y (row) := Y (row + Train_Size);
      end loop;

   end Train_Test_Split;

   --  -------------------------------------------------------------------------

end CSV_Data_Loader;
