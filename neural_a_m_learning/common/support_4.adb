
with Ada.Directories;
with Ada.Streams.Stream_IO;
with Ada.Text_IO; use Ada.Text_IO;

with NL_Types;

package body Support_4 is

   function Get_State
     (Dataset_Name : String;
      Train_X      : out Float_Matrix;
      Train_Y      : out Integer_Array;
      Test_X       : out Float_Matrix;
      Test_Y       : out Integer_Array;
      Bunch        : out Openml_Ada.Bunch_Data)
      return Boolean is
      use Ada.Directories;
      use Ada.Streams;
      use Stream_IO;
      Routine_Name   : constant String := "Support_4.Get_State ";
      Dataset_File   : constant String := "../" & Dataset_Name & ".arff";
      Save_File      : constant String := Dataset_Name & ".oml";
      State_File     : constant String := Dataset_Name & ".sta";
      Has_Data       : constant Boolean := Exists (State_File);
      Target_Columns : NL_Types.String_List;
      File_ID        : Stream_IO.File_Type;
      aStream        : Stream_Access;
      As_Frame       : Openml_Ada.As_Frame_State := Openml_Ada.As_Frame_False;
   begin
      if Has_Data then
         Put_Line (Routine_Name & "restoring state");

         Open (File_ID, In_File, State_File);
         aStream := Stream (File_ID);
         Float_Matrix'Read (aStream, Train_X);
         Integer_Array'Read (aStream, Train_Y);
         Float_Matrix'Read (aStream, Test_X);
         Integer_Array'Read (aStream, Test_Y);
         Openml_Ada.Bunch_Data'Read (aStream, Bunch);
         Close (File_ID);
         pragma Unreferenced (File_ID);

         Put_Line (Routine_Name & "state restored");
      else
         Openml_Ada.Fetch_Openml (Dataset_File_Name => Dataset_File,
                                  Save_File_Name    => Save_File,
                                  Target_Columns     => Target_Columns,
                                  Bunch             => Bunch,
                                  As_Frame          => As_Frame);
      end if;

      return Has_Data;

   end Get_State;

   --  -------------------------------------------------------------------------

   function Get_Classifier
     (Dataset_Name : String;
      Classifier   : out Multilayer_Perceptron.MLP_Classifier)
      return Boolean is
      use Ada.Directories;
      use Ada.Streams;
      use Stream_IO;
      Routine_Name : constant String := "Support_4.Get_Classifier ";
      Tree_File    : constant String := Dataset_Name & ".tre";
      Has_Tree     : constant Boolean := Exists (Tree_File);
      File_ID      : Stream_IO.File_Type;
      aStream      : Stream_Access;
   begin
      if Has_Tree then
         Put_Line (Routine_Name & "restoring classifier");

         Open (File_ID, In_File, Tree_File);
         aStream := Stream (File_ID);
         Multilayer_Perceptron.MLP_Classifier'Read (aStream, Classifier);
         Close (File_ID);
         pragma Unreferenced (File_ID);

         Put_Line (Routine_Name & "classifier restored");
      end if;

      return Has_Tree;

   end Get_Classifier;

   --  -------------------------------------------------------------------------

   procedure Save_State
     (Dataset_Name : String;
      Train_X      : Float_Matrix;
      Train_Y      : Integer_Array;
      Test_X       : Float_Matrix;
      Test_Y       : Integer_Array;
      Save_Bunch   : Openml_Ada.Bunch_Data) is
      use Ada.Streams;
      use Stream_IO;
      --        Routine_Name : constant String := "Support_4.Save_State ";
      State_File   : constant String := Dataset_Name & ".sta";
      File_ID      : Stream_IO.File_Type;
      aStream      : Stream_Access;
   begin
      Create (File_ID, Out_File, State_File);
      aStream := Stream (File_ID);
      Float_Matrix'Write (aStream, Train_X);
      Integer_Array'Write (aStream, Train_Y);
      Float_Matrix'Write (aStream, Test_X);
      Integer_Array'Write (aStream, Test_Y);
      Openml_Ada.Bunch_Data'Write (aStream, Save_Bunch);
      Close (File_ID);
      pragma Unreferenced (File_ID);

   end Save_State;

   --  -------------------------------------------------------------------------

   procedure Save_Classifier
     (Dataset_Name : String;
      Classifier   : Multilayer_Perceptron.MLP_Classifier) is
      use Ada.Streams;
      use Stream_IO;
      --        Routine_Name : constant String := "Support_4.Save_Tree ";
      Tree_File    : constant String := Dataset_Name & ".tre";
      File_ID      : Stream_IO.File_Type;
      aStream      : Stream_Access;
   begin
      Create (File_ID, Out_File, Tree_File);
      aStream := Stream (File_ID);
      Multilayer_Perceptron.MLP_Classifier'Write (aStream, Classifier);
      Close (File_ID);
      pragma Unreferenced (File_ID);

   end Save_Classifier;

end Support_4;
