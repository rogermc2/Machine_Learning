
with Ada.Streams.Stream_IO;
with Ada.Text_IO; use Ada.Text_IO;

with Tree;

package body Support_4 is

   procedure Get_State
     (Dataset_Name                 : String;
      Saved_Test_X, Saved_Test_Y,
      Saved_Train_X, Saved_Train_Y : out Value_Data_Lists_2D;
      Saved_Bunch                  : out Openml_Ada.Bunch_Data) is
      use Ada.Streams;
      use Stream_IO;
      Routine_Name : constant String := "Support_4.Get_State ";
      State_File   : constant String := Dataset_Name & ".sta";
      File_ID      : Stream_IO.File_Type;
      aStream      : Stream_Access;
   begin
      Put_Line (Routine_Name & "restoring state");

      Open (File_ID, In_File, State_File);
      aStream := Stream (File_ID);
      Value_Data_Lists_2D'Read (aStream, Saved_Test_X);
      Value_Data_Lists_2D'Read (aStream, Saved_Test_Y);
      Value_Data_Lists_2D'Read (aStream, Saved_Train_X);
      Value_Data_Lists_2D'Read (aStream, Saved_Train_Y);
      Openml_Ada.Bunch_Data'Read (aStream, Saved_Bunch);
      Close (File_ID);
      pragma Unreferenced (File_ID);

      Put_Line (Routine_Name & "state restored");

   end Get_State;

   --  -------------------------------------------------------------------------

   procedure Save_State
     (Dataset_Name               : String;
      Save_Test_X, Save_Test_Y,
      Save_Train_X, Save_Train_Y : Value_Data_Lists_2D;
      Save_Bunch                 : Openml_Ada.Bunch_Data) is
      use Ada.Streams;
      use Stream_IO;
      --        Routine_Name : constant String := "Support_4.Save_State ";
      State_File   : constant String := Dataset_Name & ".sta";
      File_ID      : Stream_IO.File_Type;
      aStream      : Stream_Access;
   begin
      Create (File_ID, Out_File, State_File);
      aStream := Stream (File_ID);
      Value_Data_Lists_2D'Write (aStream, Save_Test_X);
      Value_Data_Lists_2D'Write (aStream, Save_Test_Y);
      Value_Data_Lists_2D'Write (aStream, Save_Train_X);
      Value_Data_Lists_2D'Write (aStream, Save_Train_Y);
      Openml_Ada.Bunch_Data'Write (aStream, Save_Bunch);
      Close (File_ID);
      pragma Unreferenced (File_ID);
   end Save_State;

   --  -------------------------------------------------------------------------

   procedure Save_Tree
     (Dataset_Name : String; Classifier : Base_Decision_Tree.Classifier) is
      use Ada.Streams;
      use Stream_IO;
      --        Routine_Name : constant String := "Support_4.Save_Tree ";
      Tree_File    : constant String := Dataset_Name & ".tre";
      File_ID      : Stream_IO.File_Type;
      aStream      : Stream_Access;
   begin
      Create (File_ID, Out_File, Tree_File);
      aStream := Stream (File_ID);
      Tree.Tree_Class'Write
        (aStream, Classifier.Attributes.Decision_Tree);
      Close (File_ID);
      pragma Unreferenced (File_ID);
   end Save_Tree;

end Support_4;
