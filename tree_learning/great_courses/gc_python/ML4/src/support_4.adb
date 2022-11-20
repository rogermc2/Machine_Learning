
with Ada.Directories;
with Ada.Streams.Stream_IO;
with Ada.Text_IO; use Ada.Text_IO;

package body Support_4 is

   function Get_State
     (Dataset_Name     : String; Return_X_Y : Boolean;
      X, Y             : out Value_Data_Lists_2D;
      X_Indices        : out ML_Types.Integer_List;
      Y_Indices        : out ML_Types.Integer_List;
      Test_X, Test_Y,
      Train_X, Train_Y : out Value_Data_Lists_2D;
      Bunch            : out Openml_Ada.Bunch_Data)
      return Boolean is
      use Ada.Directories;
      use Ada.Streams;
      use Stream_IO;
      use String_Package;
      Routine_Name : constant String := "Support_4.Get_State ";
      Dataset_File : constant String := "../" & Dataset_Name & ".arff";
      Save_File    : constant String := Dataset_Name & ".oml";
      State_File   : constant String := Dataset_Name & ".sta";
      Has_Data     : constant Boolean := Exists (State_File);
      File_ID      : Stream_IO.File_Type;
      aStream      : Stream_Access;
      As_Frame     : Openml_Ada.As_Frame_State := Openml_Ada.As_Frame_False;
   begin
      if Has_Data then
         Put_Line (Routine_Name & "restoring state");

         Open (File_ID, In_File, State_File);
         aStream := Stream (File_ID);
         Value_Data_Lists_2D'Read (aStream, Test_X);
         Value_Data_Lists_2D'Read (aStream, Test_Y);
         Value_Data_Lists_2D'Read (aStream, Train_X);
         Value_Data_Lists_2D'Read (aStream, Train_Y);
         Openml_Ada.Bunch_Data'Read (aStream, Bunch);
         Close (File_ID);
         pragma Unreferenced (File_ID);

         Put_Line (Routine_Name & "state restored");
      else
         Openml_Ada.Fetch_Openml (Dataset_File_Name => Dataset_File,
                                  Save_File_Name    => Save_File,
                                  Target_Column     => Empty_List,
                                  X                 => X,
                                  Y                 => Y,
                                  X_Indices         => X_Indices,
                                  Y_Indices         => Y_Indices,
                                  Bunch             => Bunch,
                                  As_Frame          => As_Frame,
                                  Return_X_Y        => Return_X_Y);
      end if;

      return Has_Data;

   end Get_State;

   --  -------------------------------------------------------------------------

   function Get_Tree (Dataset_Name : String; theTree : out Tree.Tree_Class)
                      return Boolean is
      use Ada.Directories;
      use Ada.Streams;
      use Stream_IO;
      Routine_Name : constant String := "Support_4.Get_Tree ";
      Tree_File    : constant String := Dataset_Name & ".tre";
      Has_Tree     : constant Boolean := Exists (Tree_File);
      File_ID      : Stream_IO.File_Type;
      aStream      : Stream_Access;
   begin
      if Has_Tree then
         Put_Line (Routine_Name & "restoring tree");

         Open (File_ID, In_File, Tree_File);
         aStream := Stream (File_ID);
         Tree.Tree_Class'Read (aStream, theTree);
         Close (File_ID);
         pragma Unreferenced (File_ID);

         Put_Line (Routine_Name & "tree restored");
      end if;

      return Has_Tree;

   end Get_Tree;

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
