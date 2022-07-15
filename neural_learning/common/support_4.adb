
with Ada.Assertions; use Ada.Assertions;
with Ada.Containers;
with Ada.Directories;
with Ada.Streams.Stream_IO;
with Ada.Text_IO; use Ada.Text_IO;

with Openml_Ada;

with Data_Splitter;
with NL_Types;
--  with Printing;
with Utilities;

package body Support_4 is

   procedure Save_State
     (Dataset_Name        : String;
      Num_Features        : Positive;
      State               : Base_State;
      Save_Bunch          : Openml_Ada.Bunch_Data);

   --  -------------------------------------------------------------------------

   function Get_State
     (Dataset_Name : String; Test_Size, Train_Size : Positive)
      return Base_State is
      use Ada.Containers;
      use Ada.Directories;
      use Ada.Streams;
      use Stream_IO;
      Routine_Name   : constant String := "Support_4.Get_State ";
      Dataset_File   : constant String := "../" & Dataset_Name & ".arff";
      Save_File      : constant String := Dataset_Name & ".oml";
      State_File     : constant String := Dataset_Name & ".sta";
      Has_Data       : constant Boolean := Exists (State_File);
      Num_Features   : Positive;
      Target_Columns : NL_Types.String_List;
      Bunch          : Openml_Ada.Bunch_Data;
      File_ID        : Stream_IO.File_Type;
      aStream        : Stream_Access;
      As_Frame       : Openml_Ada.As_Frame_State := Openml_Ada.As_Frame_False;
   begin
      if Has_Data then
         Put_Line (Routine_Name & "restoring state");
         Open (File_ID, In_File, State_File);
         aStream := Stream (File_ID);
         Positive'Read (aStream, Num_Features);

         declare
            Data : Base_State (Train_Size, Test_Size, Num_Features);
         begin
            Base_State'Read (aStream, Data);
            Openml_Ada.Bunch_Data'Read (aStream, Bunch);
            Close (File_ID);
            Put_Line (Routine_Name & "state restored");
            return Data;
         end;

      else
         Put_Line (Routine_Name & "Fetch_Openml");
         Openml_Ada.Fetch_Openml (Dataset_File_Name => Dataset_File,
                                  Save_File_Name    => Save_File,
                                  Target_Columns    => Target_Columns,
                                  Bunch             => Bunch,
                                  As_Frame          => As_Frame);
         Put_Line (Routine_Name & "Openml read");

         declare
            X            : Real_Float_Matrix :=
                             To_Real_Float_Matrix (Bunch.Data);
            Y            : Integer_Array := To_Integer_Array (Bunch.Target);
            Num_Features : constant Positive := Positive (X'Length (2));
            Train_X      : Real_Float_Matrix (1 .. Train_Size,
                                              1 .. Num_Features);
            Train_Y      : Integer_Array (1 .. Train_Size);
            Train_Y2     : Integer_Matrix (1 .. Train_Size, 1 .. 1);
            Test_X       : Real_Float_Matrix (1 .. Test_Size,
                                              1 .. Num_Features);
            Test_Y       : Integer_Array (1 .. Test_Size);
            Test_Y2      : Integer_Matrix (1 .. Test_Size, 1 .. 1);
            Data         : Base_State (Train_Size, Test_Size, Num_Features);
         begin
            Put_Line (Routine_Name & "oml loaded");
            Put_Line (Routine_Name & "X Length" & Integer'Image (X'Length));
            Assert (Y'Length = X'Length, Routine_Name &
                      "Y length" & Integer'Image (Y'Length) &
                      " is different to X length" & Natural'Image (Positive (X'Length)));
            --        Printing.Print_Float_List ("Features row 16", X.Element (16));

            Put_Line (Routine_Name & "permuting");
            X := Utilities.Permute (X);
            Put_Line (Routine_Name & "X permuted");
            Utilities.Permute (Y);
            Put_Line (Routine_Name & "Y permuted");
            --        Printing.Print_Float_List ("permuted features row 16", X.Element (16));
            Put_Line (Routine_Name & "splitting data");
            Data_Splitter.Train_Test_Split
              (X => X, Y => Y, Train_Size => Train_Size, Test_Size => Test_Size,
               Train_X => Train_X, Train_Y => Train_Y,
               Test_X => Test_X, Test_Y => Test_Y);
            Put_Line ("Requested train size: " & Integer'Image (Train_Size));
            Put_Line ("Train data length: " &
                        Count_Type'Image (Train_X'Length));

            for row in Train_Y2'First .. Train_Y2'Last loop
               Train_Y2 (row, 1) := Train_Y (row);
            end loop;

            for row in Test_Y2'First .. Test_Y2'Last loop
               Test_Y2 (row, 1) := Test_Y (row);
            end loop;

            Data.Train_X := Train_X;
            Data.Train_Y := Train_Y2;
            Data.Test_X := Test_X;
            Data.Test_Y := Test_Y2;

            Save_State (Dataset_Name, Num_Features, Data, Bunch);
            return Data;
         end;
      end if;

   end Get_State;

   --  -------------------------------------------------------------------------

   procedure Save_State
     (Dataset_Name        : String;
      Num_Features        : Positive;
      State               : Base_State;
      Save_Bunch          : Openml_Ada.Bunch_Data) is
      use Ada.Streams;
      use Stream_IO;
      --        Routine_Name : constant String := "Support_4.Save_State ";
      State_File   : constant String := Dataset_Name & ".sta";
      File_ID      : Stream_IO.File_Type;
      aStream      : Stream_Access;
   begin
      Create (File_ID, Out_File, State_File);
      aStream := Stream (File_ID);
      Positive'Write (aStream, Num_Features);
      Base_State'Write (aStream, State);
      Openml_Ada.Bunch_Data'Write (aStream, Save_Bunch);
      Close (File_ID);
      pragma Unreferenced (File_ID);

   end Save_State;

   --  -------------------------------------------------------------------------

end Support_4;
