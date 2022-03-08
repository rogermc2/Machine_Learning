
with Ada.Directories;
with Ada.Streams.Stream_IO;
with Ada.Text_IO; use Ada.Text_IO;

package body Support_4 is

   function Get_State
     (Dataset_Name : String;
--        X_Indices    : out Integer_List;
--        Y_Indices    : out Integer_List;
      Train_X      : out Float_List_2D;
      Train_Y      : out Integer_List;
      Test_X       : out Float_List_2D;
      Test_Y       : out Integer_List;
      Bunch            : out Openml_Ada.Bunch_Data)
      return Boolean is
      use Ada.Directories;
      use Ada.Streams;
      use Stream_IO;
      use String_Package;
      Routine_Name   : constant String := "Support_4.Get_State ";
      Dataset_File   : constant String := "../" & Dataset_Name & ".arff";
      Save_File      : constant String := Dataset_Name & ".oml";
      State_File     : constant String := Dataset_Name & ".sta";
      Has_Data       : constant Boolean := Exists (State_File);
      Target_Columns : String_List;
      File_ID        : Stream_IO.File_Type;
      aStream        : Stream_Access;
      As_Frame       : Openml_Ada.As_Frame_State := Openml_Ada.As_Frame_False;
   begin
      if Has_Data then
         Put_Line (Routine_Name & "restoring state");

         Open (File_ID, In_File, State_File);
         aStream := Stream (File_ID);
         Float_List_2D'Read (aStream, Train_X);
         Integer_List'Read (aStream, Train_Y);
         Float_List_2D'Read (aStream, Test_X);
         Integer_List'Read (aStream, Test_Y);
         Openml_Ada.Bunch_Data'Read (aStream, Bunch);
         Close (File_ID);
         pragma Unreferenced (File_ID);

         Put_Line (Routine_Name & "state restored");
      else
         Openml_Ada.Fetch_Openml (Dataset_File_Name => Dataset_File,
                                  Save_File_Name    => Save_File,
                                  Target_Columns     => Target_Columns,
--                                    X_Indices         => X_Indices,
--                                    Y_Indices         => Y_Indices,
                                  Bunch             => Bunch,
                                  As_Frame          => As_Frame);
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
     (Dataset_Name : String;
      Train_X      : Float_List_2D;
      Train_Y      : Integer_List;
      Test_X       : Float_List_2D;
      Test_Y       : Integer_List;
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
      Float_List_2D'Write (aStream, Train_X);
      Integer_List'Write (aStream, Train_Y);
      Float_List_2D'Write (aStream, Test_X);
      Integer_List'Write (aStream, Test_Y);
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
