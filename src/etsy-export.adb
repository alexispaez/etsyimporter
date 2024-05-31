with Ada.Float_Wide_Text_IO;
with Ada.Text_IO;
with Ada.Wide_Text_IO;
with Ada.Directories; use Ada.Directories;
with Ada.Characters.Conversions; use Ada.Characters.Conversions;
with Ada.Characters.Wide_Latin_1;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
with Ada.Calendar; use Ada.Calendar;
with Ada.Strings;
with Ada.Strings.Fixed;

with Get_File;

package body Etsy.Export is

   procedure Export (Product_List : in out Products)
   is

      package WIO renames Ada.Wide_Text_IO;
      package IO renames Ada.Text_IO;
      package FWIO renames Ada.Float_Wide_Text_IO;

      Output_File       : WIO.File_Type;
      Product_Directory : constant String := "products";
      Image_Directory   : constant String := "images";
      Product_Count     : Natural := 0;
   begin

      IO.Put_Line ("Current directory: " & Current_Directory);

      --  Create directories
      if not Exists (Product_Directory) then
         Create_Directory (Product_Directory);
      end if;

      if not Exists (Image_Directory) then
         Create_Directory (Image_Directory);
      end if;

      --  Start exporting products
      for P of Product_List loop

         Product_Count := Product_Count + 1;

         --  Compose the product .md file
         declare
            File_Name : constant String :=
              Compose
                (Product_Directory,
                 "product-" & To_String (To_Wide_String (P.Sku)) & ".md");
         begin
            IO.Put_Line (File_Name);

            WIO.Create
              (File => Output_File, Mode => WIO.Out_File, Name => File_Name,
               Form => "WCEM=8");

            WIO.Put_Line (Output_File, "---");
            WIO.Put_Line
              (Output_File,
               "title: " & Ada.Characters.Wide_Latin_1.Quotation &
               To_Wide_String (P.Title) &
               Ada.Characters.Wide_Latin_1.Quotation);
            declare
               Now_Date : constant String := Image (Clock) (1 .. 10);
            begin
               WIO.Put_Line
                 (Output_File, "date: " & To_Wide_String (Now_Date));
               WIO.Put_Line
                 (Output_File, "publishdate: " & To_Wide_String (Now_Date));
            end;

            WIO.Put_Line (Output_File, "draft: true");
            WIO.Put_Line (Output_File, "type: Product");
            WIO.Put_Line
              (Output_File,
               "description: " & Ada.Characters.Wide_Latin_1.Quotation &
               To_Wide_String (P.Title) &
               Ada.Characters.Wide_Latin_1.Quotation);

            WIO.Put
              (Output_File, "price: " & Ada.Characters.Wide_Latin_1.Quotation);
            FWIO.Put (Output_File, P.Price, 2, 2, 0);
            WIO.Put (Output_File, Ada.Characters.Wide_Latin_1.Quotation);
            WIO.New_Line (Output_File);

            WIO.Put_Line
              (Output_File,
               "shortDescription: " & Ada.Characters.Wide_Latin_1.Quotation &
               To_Wide_String (P.Title) &
               Ada.Characters.Wide_Latin_1.Quotation);
            WIO.Put_Line
              (Output_File,
               "productID: " & Ada.Characters.Wide_Latin_1.Quotation &
               To_Wide_String (P.Sku) & Ada.Characters.Wide_Latin_1.Quotation);
            WIO.Put_Line (Output_File, "pngproduct: [category]");
            WIO.Put_Line
              (Output_File,
               "fileGUID: " & Ada.Characters.Wide_Latin_1.Quotation &
               Ada.Characters.Wide_Latin_1.Quotation);
            WIO.Put_Line (Output_File, "---");
            WIO.Put_Line (Output_File, To_Wide_String (P.Description));

            Ada.Wide_Text_IO.Close (Output_File);

            --  Get all the product's image files
            --  Compose image file names with product code and image number
            declare
               Counter : Positive := 1;
            begin
               for Image of P.Images loop
                  if Image /= Null_Unbounded_Wide_String then
                     declare
                        File_Name : constant String :=
                          "product-" &
                          To_String (To_Wide_String (P.Sku)) &
                          "-" &
                          Ada.Strings.Fixed.Tail
                          (Ada.Strings.Fixed.Trim
                             (Positive'Image (Counter),
                              Ada.Strings.Left), 3, '0') &
                          ".jpg";
                     begin
                        Ada.Text_IO.Put_Line ("File name: " & File_Name);
                        Get_File (To_String (To_Wide_String (Image)),
                                  Compose (Image_Directory, File_Name));
                     end;

                     Ada.Text_IO.Put_Line
                       ("File " &
                          Ada.Strings.Fixed.Tail
                          (Ada.Strings.Fixed.Trim
                               (Positive'Image (Counter),
                                Ada.Strings.Left), 3, '0') &
                          " " &
                          To_String (To_Wide_String (Image)));
                     Counter := Counter + 1;
                  end if;
               end loop;
            end;

            --  exit when Product_Count = 3;

         end;

      end loop;

   end Export;

end Etsy.Export;
