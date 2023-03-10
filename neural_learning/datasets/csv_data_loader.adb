
with Ada.Assertions; use Ada.Assertions;
with Ada.Directories;
with Ada.Streams.Stream_IO;
with Ada.Text_IO; use Ada.Text_IO;

--  with Basic_Printing; use Basic_Printing;
with Load_Dataset;
with Shuffler;

package body CSV_Data_Loader is

   type Digits_XY_Data (Num_Items, Num_Features : Positive) is record
      X : Real_Float_Matrix (1 .. Num_Items, 1 .. Num_Features);
      Y : Integer_Array (1 .. Num_Items);
   end record;

   function Load_Data_Set (File_Name : String; Num_Classes : Natural := 10;
                           Max_Lines : Positive := 20000)
                           return Load_Dataset.Digits_Data_Record;
   procedure Save_State (Dataset_Name : String; State : Base_Split_State;
                         Num_Features : Positive);
   procedure Train_Test_Split
     (X          : Real_Float_Matrix; Y : Integer_Array;
      Train_Size : Natural; Test_Size  : Natural;
      Train_X    : out Real_Float_Matrix; Train_Y : out Integer_Array;
      Test_X     : out Real_Float_Matrix; Test_Y : out Integer_Array);

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

   function Fetch_Digits_Data (Name : String) return Digits_XY_Data is
      Routine_Name : constant String := "CSV_Data_Loader.Fetch_Digits_Data ";
      Data_Record  : constant Load_Dataset.Digits_Data_Record :=
                       Load_Data_Set (Name & ".csv");
      Data         : Digits_XY_Data (Data_Record.Num_Samples,
                                     Data_Record.Num_Features);
   begin
      Put_Line (Routine_Name);
      Assert (Data_Record.Target'Length = Data_Record.Features'Length,
              Routine_Name & "Target length" &
                Integer'Image (Data_Record.Target'Length) &
                " is different to Features length" &
                Natural'Image (Positive (Data_Record.Features'Length)));

      for row in Data_Record.Features'Range loop
         for col in Data_Record.Features'Range (2) loop
            Data.X (row, col) := Float (Data_Record.Features  (row, col)) / 255.0;
         end loop;
      end loop;
      Data.Y := Data_Record.Target;

      return Data;

   end Fetch_Digits_Data;

   --  -------------------------------------------------------------------------

   function Get_Diabetes_Split_State
     (File_Name  : String; Train_Size, Test_Size : Positive;
      Shuffle    : Boolean := True; Reload : Boolean := False)
      return Base_Split_State is
      use Ada.Directories;
      use Ada.Streams;
      use Stream_IO;
      Routine_Name   : constant String :=
                         "CSV_Data_Loader.Get_Diabetes_Split_State ";
      State_File     : constant String := File_Name & ".sta";
      Has_Data       : constant Boolean := Exists (State_File);
      Num_Features   : Positive;
      File_ID        : Stream_IO.File_Type;
      aStream        : Stream_Access;
   begin
      if Has_Data and not Reload then
         Put_Line (Routine_Name & "restoring state");
         Open (File_ID, In_File, State_File);
         aStream := Stream (File_ID);
         Positive'Read (aStream, Num_Features);

         declare
            Data : Base_Split_State
              (Train_Size, Test_Size, Num_Features, False);
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
            Data_Record  : constant Load_Dataset.Diabetes_Data_Record :=
                             Load_Dataset.Load_Diabetes (File_Name);
            X            : Real_Float_Matrix :=
                             To_Real_Float_Matrix (Data_Record.Features);
            Y            : Integer_Array :=
                             To_Integer_Array (Data_Record.Target);
            Train_Y      : Integer_Array (1 .. Train_Size);
            Test_Y       : Integer_Array (1 .. Test_Size);
            Data         : Base_Split_State (Train_Size, Test_Size,
                                             X'Length (2), False);
         begin
            Put_Line (Routine_Name & "csv loaded");
            Num_Features := X'Length (2);
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
               Train_X => Data.Train_X , Train_Y => Train_Y,
               Test_X => Data.Test_X, Test_Y => Test_Y);
            Data.Train_Y := To_Integer_Matrix (Train_Y);
            Data.Test_Y := To_Integer_Matrix (Test_Y);

            Save_State (File_Name, Data, Num_Features);
            return Data;
         end;
      end if;

   end Get_Diabetes_Split_State;

   --  -------------------------------------------------------------------------

   function Get_Digits_Split_State
     (Dataset_Name  : String; Train_Size, Test_Size : Positive;
      Y_Categorized : Boolean := True; Shuffle : Boolean := True;
      Reload        : Boolean := False)
      return Base_Split_State is
      use Ada.Directories;
      use Ada.Streams;
      use Stream_IO;
      Routine_Name   : constant String :=
                         "CSV_Data_Loader.Get_Digits_Split_State ";
      State_File     : constant String := Dataset_Name & ".sta";
      Has_Data       : constant Boolean := Exists (State_File);
      Num_Features   : Positive;
      File_ID        : Stream_IO.File_Type;
      aStream        : Stream_Access;
   begin
      if Has_Data and not Reload then
         Put_Line (Routine_Name & "restoring state");
         Open (File_ID, In_File, State_File);
         aStream := Stream (File_ID);
         Positive'Read (aStream, Num_Features);

         declare
            Data : Base_Split_State (Train_Size, Test_Size, Num_Features,
                                     Y_Categorized);
         begin
            Base_Split_State'Read (aStream, Data);
            Close (File_ID);
            Put_Line (Routine_Name & "state restored");
            return Data;
         end;

      else
         Put_Line (Routine_Name & "fetching data");
         declare
            XY_Data : Digits_XY_Data := Fetch_Digits_Data (Dataset_Name);
            Train_Y : Integer_Array (1 .. Train_Size);
            Test_Y  : Integer_Array (1 .. Test_Size);
         begin
            Put_Line (Routine_Name & Dataset_Name & ".csv loaded");
            declare
               Data  : Base_Split_State (Train_Size, Test_Size,
                                         XY_Data.Num_Features, Y_Categorized);
            begin
               Put_Line (Routine_Name & "Data initialized");

               if Shuffle then
                  Put_Line (Routine_Name & "shuffling");
                  Shuffler.Shuffle (XY_Data.X, XY_Data.Y);
               end if;

               Put_Line (Routine_Name & "splitting data");
               Train_Test_Split
                 (X => XY_Data.X, Y => XY_Data.Y, Train_Size => Train_Size,
                  Test_Size => Test_Size,
                  Train_X => Data.Train_X , Train_Y => Train_Y,
                  Test_X => Data.Test_X, Test_Y => Test_Y);

               if Y_Categorized then
                  Data.Cat_Train_Y := Categorize (Train_Y);
                  for index in Test_Y'Range loop
                     Data.Cat_Test_Y (index) := Float (Test_Y (index));
                  end loop;
               else
                  Data.Train_Y := To_Integer_Matrix (Train_Y);
                  Data.Test_Y := To_Integer_Matrix (Test_Y);
               end if;

               Save_State (Dataset_Name, Data, Num_Features);
               return Data;
            end;
         end;
      end if;

   end Get_Digits_Split_State;

   --  -------------------------------------------------------------------------

   function Get_Iris_Split_State
     (File_Name  : String; Train_Size, Test_Size : Positive;
      Shuffle    : Boolean := True; Reload : Boolean := False)
      return Base_Split_State is
      use Ada.Directories;
      use Ada.Streams;
      use Stream_IO;
      Routine_Name   : constant String :=
                         "CSV_Data_Loader.Get_Iris_Split_State ";
      State_File     : constant String := File_Name & ".sta";
      Has_Data       : constant Boolean := Exists (State_File);
      Num_Features   : Positive;
      File_ID        : Stream_IO.File_Type;
      aStream        : Stream_Access;
   begin
      if Has_Data and not Reload then
         Put_Line (Routine_Name & "restoring state");
         Open (File_ID, In_File, State_File);
         aStream := Stream (File_ID);
         Positive'Read (aStream, Num_Features);

         declare
            Data : Base_Split_State (Train_Size, Test_Size, Num_Features, False);
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
            Data_Record  : constant Load_Dataset.Iris_Data_Record :=
                             Load_Dataset.Load_Iris (File_Name);
            X            : Real_Float_Matrix :=
                             To_Real_Float_Matrix (Data_Record.Features);
            Y            : Integer_Array :=
                             To_Integer_Array (Data_Record.Target);
            Train_Y      : Integer_Array (1 .. Train_Size);
            Test_Y       : Integer_Array (1 .. Test_Size);
            Data         : Base_Split_State (Train_Size, Test_Size,
                                             X'Length (2), False);
         begin
            Put_Line (Routine_Name & "csv loaded");
            Num_Features := X'Length (2);
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
               Train_X => Data.Train_X , Train_Y => Train_Y,
               Test_X => Data.Test_X, Test_Y => Test_Y);
            Data.Train_Y := To_Integer_Matrix (Train_Y);
            Data.Test_Y := To_Integer_Matrix (Test_Y);

            Save_State (File_Name, Data, Num_Features);
            return Data;
         end;
      end if;

   end Get_Iris_Split_State;

   --  -------------------------------------------------------------------------

   function Get_Ship_Split_State
     (File_Name  : String; Train_Size, Test_Size : Positive;
      Shuffle    : Boolean := True; Reload : Boolean := False)
      return Base_Split_State is
      use Ada.Directories;
      use Ada.Streams;
      use Stream_IO;
      Routine_Name   : constant String := "CSV_Data_Loader.Get_Ship_Split_State ";
      State_File     : constant String := File_Name & ".sta";
      Has_Data       : constant Boolean := Exists (State_File);
      Num_Features   : Positive;
      File_ID        : Stream_IO.File_Type;
      aStream        : Stream_Access;
   begin
      if Has_Data and not Reload then
         Put_Line (Routine_Name & "restoring state");
         Open (File_ID, In_File, State_File);
         aStream := Stream (File_ID);
         Positive'Read (aStream, Num_Features);

         declare
            Data : Base_Split_State
              (Train_Size, Test_Size, Num_Features, False);
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
            Data_Record  : constant Load_Dataset.Diabetes_Data_Record :=
                             Load_Dataset.Load_Diabetes (File_Name);
            X            : Real_Float_Matrix :=
                             To_Real_Float_Matrix (Data_Record.Features);
            Y            : Integer_Array :=
                             To_Integer_Array (Data_Record.Target);
            Train_Y      : Integer_Array (1 .. Train_Size);
            Test_Y       : Integer_Array (1 .. Test_Size);
            Data         : Base_Split_State (Train_Size, Test_Size,
                                             X'Length (2), False);
         begin
            Put_Line (Routine_Name & "csv loaded");
            Num_Features := X'Length (2);
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
               Train_X => Data.Train_X , Train_Y => Train_Y,
               Test_X => Data.Test_X, Test_Y => Test_Y);
            Data.Train_Y := To_Integer_Matrix (Train_Y);
            Data.Test_Y := To_Integer_Matrix (Test_Y);

            Save_State (File_Name, Data, Num_Features);
            return Data;
         end;
      end if;

   end Get_Ship_Split_State;

   --  -------------------------------------------------------------------------

   function Get_Split_State
     (File_Name                      : String; Data_Type : Data_Kind;
      Train_Size                     : Positive; Test_Size : Positive;
      Y_Categorized, Shuffle, Reload : Boolean := False)
      return Base_Split_State is
      --        Routine_Name   : constant String := "CSV_Data_Loader.Get_Split_State ";
      Dummy_Data     : Base_Split_State (Train_Size, Test_Size, 1, True);
   begin
      case Data_Type is
      when Diabetes_Data =>
         return Get_Diabetes_Split_State (File_Name, Train_Size, Test_Size,
                                          Shuffle, Reload);

      when Digits_Data =>
         return Get_Digits_Split_State (File_Name, Train_Size, Test_Size,
                                        Y_Categorized, Shuffle, Reload);
      when Iris_Data =>
         return Get_Iris_Split_State (File_Name, Train_Size, Test_Size,
                                      Shuffle, Reload);
      when Ship_Data =>
         return Get_Ship_Split_State (File_Name, Train_Size, Test_Size,
                                      Shuffle, Reload);
      end case;

   end Get_Split_State;

   --  -------------------------------------------------------------------------

   function Load_Data_Set (File_Name : String; Num_Classes : Natural := 10;
                           Max_Lines : Positive := 20000)
                           return Load_Dataset.Digits_Data_Record is
      use Load_Dataset;
      --        Routine_Name   : constant String := "CSV_Data_Loader.Load_Data_Set ";
      Data           : constant Digits_Data_Record :=
                         Load_Digits (File_Name, Num_Classes, Max_Lines);
   begin
      return Data;

   end Load_Data_Set;

   --  -------------------------------------------------------------------------

   procedure Save_State (Dataset_Name : String; State : Base_Split_State;
                         Num_Features : Positive) is
      use Ada.Streams;
      use Stream_IO;
      --        Routine_Name : constant String := "CSV_Data_Loader.Save_State ";
      State_File   : constant String := Dataset_Name & ".sta";
      File_ID      : Stream_IO.File_Type;
      aStream      : Stream_Access;
   begin
      Create (File_ID, Out_File, State_File);
      aStream := Stream (File_ID);
      Positive'Write (aStream, Num_Features);
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
