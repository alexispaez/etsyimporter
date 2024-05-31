with Ada.Wide_Text_IO;
with Ada.Float_Wide_Text_IO;
with Ada.Integer_Wide_Text_IO;
with Ada.IO_Exceptions; use Ada.IO_Exceptions;
with Ada.Characters.Conversions; use Ada.Characters.Conversions;

package body Etsy.Import is

   procedure Set_Field_From_String
     (P             : in out Product;
      Field_Type    : Product_CSV_Fields;
      String_Buffer : Unbounded_Wide_String)
   is
      package FWIO renames Ada.Float_Wide_Text_IO;
      package IWIO renames Ada.Integer_Wide_Text_IO;
   begin
      case Field_Type is
         when Title =>
            P.Title := String_Buffer;
         when Description =>
            P.Description := String_Buffer;
         when Price =>
            declare
               Value  : Float;
               Length : Natural;
            begin
               begin
                  FWIO.Get (To_Wide_String (String_Buffer), Value, Length);
               exception
                  when Data_Error =>
                     Value := 0.0;
               end;

               P.Price := Value;
            end;
         when Currency_Code =>
            P.Currency_Code :=
              Ada.Characters.Conversions.To_String
                (To_Wide_String (String_Buffer));
         when Quantity =>
            declare
               Value  : Integer;
               Length : Natural;
            begin
               begin
                  IWIO.Get (To_Wide_String (String_Buffer), Value, Length);
               exception
                  when Data_Error =>
                     Value := 0;
               end;

               P.Quantity := Value;
            end;
         when Tags =>
            P.Tags := String_Buffer;
         when Materials =>
            P.Materials := String_Buffer;
         when Image_1 =>
            P.Images (1) := String_Buffer;
         when Image_2 =>
            P.Images (2) := String_Buffer;
         when Image_3 =>
            P.Images (3) := String_Buffer;
         when Image_4 =>
            P.Images (4) := String_Buffer;
         when Image_5 =>
            P.Images (5) := String_Buffer;
         when Image_6 =>
            P.Images (6) := String_Buffer;
         when Image_7 =>
            P.Images (7) := String_Buffer;
         when Image_8 =>
            P.Images (8) := String_Buffer;
         when Image_9 =>
            P.Images (9) := String_Buffer;
         when Image_10 =>
            P.Images (10) := String_Buffer;
         when Variation_1_Type =>
            P.Variation_1.Variation_Type := String_Buffer;
         when Variation_1_Name =>
            P.Variation_1.Name := String_Buffer;
         when Variation_1_Values =>
            P.Variation_1.Values := String_Buffer;
         when Variation_2_Type =>
            P.Variation_2.Variation_Type := String_Buffer;
         when Variation_2_Name =>
            P.Variation_2.Name := String_Buffer;
         when Variation_2_Values =>
            P.Variation_2.Values := String_Buffer;
         when Sku =>
            P.Sku := String_Buffer;
      end case;
   end Set_Field_From_String;

   function Next (F : Product_CSV_Fields) return Product_CSV_Fields is
   begin
      return
        (if F = Product_CSV_Fields'Last then
            Product_CSV_Fields'First
         else
            Product_CSV_Fields'Succ (F));
   end Next;

   procedure Set_Field
     (P            : in out Product;
      Field        : Product_CSV_Fields;
      Field_Buffer : in out Unbounded_Wide_String)
   is
   begin
      if Field = Product_CSV_Fields'First then
         P.Init;
      end if;

      Set_Field_From_String (P, Field, Field_Buffer);

      Field_Buffer := Null_Unbounded_Wide_String;
   end Set_Field;

   procedure Import (Product_List : in out Products; File_Name : String) is

      package WIO renames Ada.Wide_Text_IO;

      Input_File    : WIO.File_Type;
      In_String     : Boolean            := False;
      In_Header     : Boolean            := True;
      Field_Buffer  : Unbounded_Wide_String;
      Current_Field : Product_CSV_Fields := Product_CSV_Fields'First;
      P             : Product;
   begin
      WIO.Open (File => Input_File,
                Mode => WIO.In_File,
                Name => File_Name,
                Form => "WCEM=8");

      while not WIO.End_Of_File (Input_File) loop

         declare
            Input_Line : constant Wide_String := WIO.Get_Line (Input_File);
         begin

            if Input_Line'Length /= 0 then

               --  Process one character at a time
               for C of Input_Line loop
                  case C is
                     when '"' =>           --  Found start or end of a string

                        if In_String then
                           In_String := False;
                        else
                           In_String := True;
                        end if;

                     when ',' =>           --  Found a separator

                        if In_Header then
                           if Current_Field = Product_CSV_Fields'Last then
                              In_Header := False;
                           end if;
                           Current_Field := Next (Current_Field);
                        else
                           if In_String then
                              Field_Buffer := Field_Buffer & C;
                           else
                              Set_Field (P, Current_Field, Field_Buffer);
                              if Current_Field = Product_CSV_Fields'Last then
                                 Product_List.Append (P);
                              end if;
                              Current_Field := Next (Current_Field);
                           end if;
                        end if;

                     when others =>        --  Found normal character

                        if not In_Header then
                           Field_Buffer := Field_Buffer & C;
                        end if;

                  end case;
               end loop;

               --  Completed processing the line
               if not In_Header then
                  if In_String then
                     --  Output a new line character
                     Field_Buffer := Field_Buffer
                     --  & Wide_Character'Val (0_013)
                     & Wide_Character'Val (0_010);
                  else
                     Set_Field (P, Current_Field, Field_Buffer);
                     if Current_Field = Product_CSV_Fields'Last then
                        Product_List.Append (P);
                     end if;
                     Current_Field := Next (Current_Field);
                  end if;
               end if;

            else

               --  Empty line
               if not In_Header then
                  if In_String then
                     Field_Buffer := Field_Buffer
                       & Wide_Character'Val (0_010);
                  end if;
               end if;

            end if;  --  if Input_Line'Length /= 0

         end;

      end loop;  -- File loop

      WIO.Close (Input_File);

   end Import;

end Etsy.Import;
