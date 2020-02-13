/* gtkpango.c - pango-related utilities
 *
 * Copyright (c) 2010 Red Hat, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library. If not, see <http://www.gnu.org/licenses/>.Free
 */
/*
 * Modified by the GTK+ Team and others 1997-2000.  See the AUTHORS
 * file for a list of people on the GTK+ Team.  See the ChangeLog
 * files for a list of changes.  These files are distributed with
 * GTK+ at ftp://ftp.gtk.org/pub/gtk/.
 */

#include "config.h"
#include "gtkpango.h"
#include <pango/pangocairo.h>
#include <fribidi.h>
#include "gtkintl.h"

#define GTK_TYPE_FILL_LAYOUT_RENDERER            (_gtk_fill_layout_renderer_get_type())
#define GTK_FILL_LAYOUT_RENDERER(object)         (G_TYPE_CHECK_INSTANCE_CAST ((object), GTK_TYPE_FILL_LAYOUT_RENDERER, GtkFillLayoutRenderer))
#define GTK_IS_FILL_LAYOUT_RENDERER(object)      (G_TYPE_CHECK_INSTANCE_TYPE ((object), GTK_TYPE_FILL_LAYOUT_RENDERER))
#define GTK_FILL_LAYOUT_RENDERER_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GTK_TYPE_FILL_LAYOUT_RENDERER, GtkFillLayoutRendererClass))
#define GTK_IS_FILL_LAYOUT_RENDERER_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GTK_TYPE_FILL_LAYOUT_RENDERER))
#define GTK_FILL_LAYOUT_RENDERER_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GTK_TYPE_FILL_LAYOUT_RENDERER, GtkFillLayoutRendererClass))

typedef struct _GtkFillLayoutRenderer      GtkFillLayoutRenderer;
typedef struct _GtkFillLayoutRendererClass GtkFillLayoutRendererClass;

struct _GtkFillLayoutRenderer
{
  PangoRenderer parent_instance;

  cairo_t *cr;
};

struct _GtkFillLayoutRendererClass
{
  PangoRendererClass parent_class;
};

GType _gtk_fill_layout_renderer_get_type (void);

G_DEFINE_TYPE (GtkFillLayoutRenderer, _gtk_fill_layout_renderer, PANGO_TYPE_RENDERER)

static void
gtk_fill_layout_renderer_draw_glyphs (PangoRenderer     *renderer,
                                      PangoFont         *font,
                                      PangoGlyphString  *glyphs,
                                      int                x,
                                      int                y)
{
  GtkFillLayoutRenderer *text_renderer = GTK_FILL_LAYOUT_RENDERER (renderer);

  cairo_move_to (text_renderer->cr, (double)x / PANGO_SCALE, (double)y / PANGO_SCALE);
  pango_cairo_show_glyph_string (text_renderer->cr, font, glyphs);
}

static void
gtk_fill_layout_renderer_draw_glyph_item (PangoRenderer     *renderer,
                                          const char        *text,
                                          PangoGlyphItem    *glyph_item,
                                          int                x,
                                          int                y)
{
  GtkFillLayoutRenderer *text_renderer = GTK_FILL_LAYOUT_RENDERER (renderer);

  cairo_move_to (text_renderer->cr, (double)x / PANGO_SCALE, (double)y / PANGO_SCALE);
  pango_cairo_show_glyph_item (text_renderer->cr, text, glyph_item);
}

static void
gtk_fill_layout_renderer_draw_rectangle (PangoRenderer     *renderer,
                                         PangoRenderPart    part,
                                         int                x,
                                         int                y,
                                         int                width,
                                         int                height)
{
  GtkFillLayoutRenderer *text_renderer = GTK_FILL_LAYOUT_RENDERER (renderer);

  if (part == PANGO_RENDER_PART_BACKGROUND)
    return;

  cairo_rectangle (text_renderer->cr,
                   (double)x / PANGO_SCALE, (double)y / PANGO_SCALE,
		   (double)width / PANGO_SCALE, (double)height / PANGO_SCALE);
  cairo_fill (text_renderer->cr);
}

static void
gtk_fill_layout_renderer_draw_trapezoid (PangoRenderer     *renderer,
                                         PangoRenderPart    part,
                                         double             y1_,
                                         double             x11,
                                         double             x21,
                                         double             y2,
                                         double             x12,
                                         double             x22)
{
  GtkFillLayoutRenderer *text_renderer = GTK_FILL_LAYOUT_RENDERER (renderer);
  cairo_matrix_t matrix;
  cairo_t *cr;

  cr = text_renderer->cr;

  cairo_save (cr);

  /* use identity scale, but keep translation */
  cairo_get_matrix (cr, &matrix);
  matrix.xx = matrix.yy = 1;
  matrix.xy = matrix.yx = 0;
  cairo_set_matrix (cr, &matrix);

  cairo_move_to (cr, x11, y1_);
  cairo_line_to (cr, x21, y1_);
  cairo_line_to (cr, x22, y2);
  cairo_line_to (cr, x12, y2);
  cairo_close_path (cr);

  cairo_fill (cr);

  cairo_restore (cr);
}

static void
gtk_fill_layout_renderer_draw_error_underline (PangoRenderer *renderer,
                                               int            x,
                                               int            y,
                                               int            width,
                                               int            height)
{
  GtkFillLayoutRenderer *text_renderer = GTK_FILL_LAYOUT_RENDERER (renderer);

  pango_cairo_show_error_underline (text_renderer->cr,
                                    (double)x / PANGO_SCALE, (double)y / PANGO_SCALE,
                                    (double)width / PANGO_SCALE, (double)height / PANGO_SCALE);
}

static void
gtk_fill_layout_renderer_draw_shape (PangoRenderer   *renderer,
                                     PangoAttrShape  *attr,
                                     int              x,
                                     int              y)
{
  GtkFillLayoutRenderer *text_renderer = GTK_FILL_LAYOUT_RENDERER (renderer);
  cairo_t *cr = text_renderer->cr;
  PangoLayout *layout;
  PangoCairoShapeRendererFunc shape_renderer;
  gpointer                    shape_renderer_data;

  layout = pango_renderer_get_layout (renderer);

  if (!layout)
  	return;

  shape_renderer = pango_cairo_context_get_shape_renderer (pango_layout_get_context (layout),
							   &shape_renderer_data);

  if (!shape_renderer)
    return;

  cairo_save (cr);

  cairo_move_to (cr, (double)x / PANGO_SCALE, (double)y / PANGO_SCALE);

  shape_renderer (cr, attr, FALSE, shape_renderer_data);

  cairo_restore (cr);
}

static void
gtk_fill_layout_renderer_finalize (GObject *object)
{
  G_OBJECT_CLASS (_gtk_fill_layout_renderer_parent_class)->finalize (object);
}

static void
_gtk_fill_layout_renderer_init (GtkFillLayoutRenderer *renderer)
{
}

static void
_gtk_fill_layout_renderer_class_init (GtkFillLayoutRendererClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);
  
  PangoRendererClass *renderer_class = PANGO_RENDERER_CLASS (klass);
  
  renderer_class->draw_glyphs = gtk_fill_layout_renderer_draw_glyphs;
  renderer_class->draw_glyph_item = gtk_fill_layout_renderer_draw_glyph_item;
  renderer_class->draw_rectangle = gtk_fill_layout_renderer_draw_rectangle;
  renderer_class->draw_trapezoid = gtk_fill_layout_renderer_draw_trapezoid;
  renderer_class->draw_error_underline = gtk_fill_layout_renderer_draw_error_underline;
  renderer_class->draw_shape = gtk_fill_layout_renderer_draw_shape;

  object_class->finalize = gtk_fill_layout_renderer_finalize;
}

void
_gtk_pango_fill_layout (cairo_t     *cr,
                        PangoLayout *layout)
{
  static GtkFillLayoutRenderer *renderer = NULL;
  gboolean has_current_point;
  double current_x, current_y;

  has_current_point = cairo_has_current_point (cr);
  cairo_get_current_point (cr, &current_x, &current_y);

  if (renderer == NULL)
    renderer = g_object_new (GTK_TYPE_FILL_LAYOUT_RENDERER, NULL);

  cairo_save (cr);
  cairo_translate (cr, current_x, current_y);

  renderer->cr = cr;
  pango_renderer_draw_layout (PANGO_RENDERER (renderer), layout, 0, 0);

  cairo_restore (cr);

  if (has_current_point)
    cairo_move_to (cr, current_x, current_y);
}

/*
 * _gtk_pango_get_run_attributes:
 * @attributes: a #AtkAttributeSet to add attributes to
 * @layout: the #PangoLayout to get the attributes from
 * @offset: the offset at which the attributes are wanted
 * @start_offset: return location for the starting offset
 *    of the current run
 * @end_offset: return location for the ending offset of the
 *    current run
 *
 * Finds the “run” around index (i.e. the maximal range of characters
 * where the set of applicable attributes remains constant) and
 * returns the starting and ending offsets for it.
 *
 * The attributes for the run are added to @attributes, after
 * translating them from Pango attributes to ATK attributes.
 *
 * This is a convenience function that can be used to implement
 * support for the #AtkText interface in widgets using Pango
 * layouts.
 *
 * Returns: the modified #AtkAttributeSet
 */
AtkAttributeSet *
_gtk_pango_get_run_attributes (AtkAttributeSet *attributes,
                               PangoLayout     *layout,
                               gint             offset,
                               gint            *start_offset,
                               gint            *end_offset)
{
  PangoAttrIterator *iter;
  PangoAttrList *attr;
  PangoAttrString *pango_string;
  PangoAttrInt *pango_int;
  PangoAttrColor *pango_color;
  PangoAttrLanguage *pango_lang;
  PangoAttrFloat *pango_float;
  gint index, start_index, end_index;
  gboolean is_next;
  glong len;
  const gchar *text;
  gchar *value;

  text = pango_layout_get_text (layout);
  len = g_utf8_strlen (text, -1);

  *start_offset = 0;
  *end_offset = len;
  return attributes;
}

/*
 * _gtk_pango_move_chars:
 * @layout: a #PangoLayout
 * @offset: a character offset in @layout
 * @count: the number of characters to move from @offset
 *
 * Returns the position that is @count characters from the
 * given @offset. @count may be positive or negative.
 *
 * For the purpose of this function, characters are defined
 * by what Pango considers cursor positions.
 *
 * Returns: the new position
 */
gint
_gtk_pango_move_chars (PangoLayout *layout,
                       gint         offset,
                       gint         count)
{
  const PangoLogAttr *attrs;
  gint n_attrs;

  attrs = pango_layout_get_log_attrs_readonly (layout, &n_attrs);

  while (count > 0 && offset < n_attrs - 1)
    {
      do
        offset++;
      while (offset < n_attrs - 1 && !attrs[offset].is_cursor_position);

      count--;
    }
  while (count < 0 && offset > 0)
    {
      do
        offset--;
      while (offset > 0 && !attrs[offset].is_cursor_position);

      count++;
    }

  return offset;
}

/*
 * _gtk_pango_move_words:
 * @layout: a #PangoLayout
 * @offset: a character offset in @layout
 * @count: the number of words to move from @offset
 *
 * Returns the position that is @count words from the
 * given @offset. @count may be positive or negative.
 *
 * If @count is positive, the returned position will
 * be a word end, otherwise it will be a word start.
 * See the Pango documentation for details on how
 * word starts and ends are defined.
 *
 * Returns: the new position
 */
gint
_gtk_pango_move_words (PangoLayout  *layout,
                       gint          offset,
                       gint          count)
{
  const PangoLogAttr *attrs;
  gint n_attrs;

  attrs = pango_layout_get_log_attrs_readonly (layout, &n_attrs);

  while (count > 0 && offset < n_attrs - 1)
    {
      do
        offset++;
      while (offset < n_attrs - 1 && !attrs[offset].is_word_end);

      count--;
    }
  while (count < 0 && offset > 0)
    {
      do
        offset--;
      while (offset > 0 && !attrs[offset].is_word_start);

      count++;
    }

  return offset;
}

/*
 * _gtk_pango_move_sentences:
 * @layout: a #PangoLayout
 * @offset: a character offset in @layout
 * @count: the number of sentences to move from @offset
 *
 * Returns the position that is @count sentences from the
 * given @offset. @count may be positive or negative.
 *
 * If @count is positive, the returned position will
 * be a sentence end, otherwise it will be a sentence start.
 * See the Pango documentation for details on how
 * sentence starts and ends are defined.
 *
 * Returns: the new position
 */
gint
_gtk_pango_move_sentences (PangoLayout  *layout,
                           gint          offset,
                           gint          count)
{
  const PangoLogAttr *attrs;
  gint n_attrs;

  attrs = pango_layout_get_log_attrs_readonly (layout, &n_attrs);

  while (count > 0 && offset < n_attrs - 1)
    {
      do
        offset++;
      while (offset < n_attrs - 1 && !attrs[offset].is_sentence_end);

      count--;
    }
  while (count < 0 && offset > 0)
    {
      do
        offset--;
      while (offset > 0 && !attrs[offset].is_sentence_start);

      count++;
    }

  return offset;
}

/*
 * _gtk_pango_move_lines:
 * @layout: a #PangoLayout
 * @offset: a character offset in @layout
 * @count: the number of lines to move from @offset
 *
 * Returns the position that is @count lines from the
 * given @offset. @count may be positive or negative.
 *
 * If @count is negative, the returned position will
 * be the start of a line, else it will be the end of
 * line.
 *
 * Returns: the new position
 */
gint
_gtk_pango_move_lines (PangoLayout *layout,
                       gint         offset,
                       gint         count)
{
  GSList *lines, *l;
  PangoLayoutLine *line;
  gint num;
  const gchar *text;
  gint pos, line_pos;
  gint index;
  gint len;

  text = pango_layout_get_text (layout);
  index = g_utf8_offset_to_pointer (text, offset) - text;
  lines = pango_layout_get_lines (layout);
  line = NULL;

  num = 0;
  for (l = lines; l; l = l->next)
    {
      line = l->data;
      if (index < line->start_index + line->length)
        break;
      num++;
    }

  if (count < 0)
    {
      num += count;
      if (num < 0)
        num = 0;

      line = g_slist_nth_data (lines, num);

      return g_utf8_pointer_to_offset (text, text + line->start_index);
    }
  else
    {
      line_pos = index - line->start_index;

      len = g_slist_length (lines);
      num += count;
      if (num >= len || (count == 0 && num == len - 1))
        return g_utf8_strlen (text, -1) - 1;

      line = l->data;
      pos = line->start_index + line_pos;
      if (pos >= line->start_index + line->length)
        pos = line->start_index + line->length - 1;

      return g_utf8_pointer_to_offset (text, text + pos);
    }
}

/*
 * _gtk_pango_is_inside_word:
 * @layout: a #PangoLayout
 * @offset: a character offset in @layout
 *
 * Returns whether the given position is inside
 * a word.
 *
 * Returns: %TRUE if @offset is inside a word
 */
gboolean
_gtk_pango_is_inside_word (PangoLayout  *layout,
                           gint          offset)
{
  const PangoLogAttr *attrs;
  gint n_attrs;

  attrs = pango_layout_get_log_attrs_readonly (layout, &n_attrs);

  while (offset >= 0 &&
         !(attrs[offset].is_word_start || attrs[offset].is_word_end))
    offset--;

  if (offset >= 0)
    return attrs[offset].is_word_start;

  return FALSE;
}

/*
 * _gtk_pango_is_inside_sentence:
 * @layout: a #PangoLayout
 * @offset: a character offset in @layout
 *
 * Returns whether the given position is inside
 * a sentence.
 *
 * Returns: %TRUE if @offset is inside a sentence
 */
gboolean
_gtk_pango_is_inside_sentence (PangoLayout  *layout,
                               gint          offset)
{
  const PangoLogAttr *attrs;
  gint n_attrs;

  attrs = pango_layout_get_log_attrs_readonly (layout, &n_attrs);

  while (offset >= 0 &&
         !(attrs[offset].is_sentence_start || attrs[offset].is_sentence_end))
    offset--;

  if (offset >= 0)
    return attrs[offset].is_sentence_start;

  return FALSE;
}

static void
pango_layout_get_line_before (PangoLayout     *layout,
                              AtkTextBoundary  boundary_type,
                              gint             offset,
                              gint            *start_offset,
                              gint            *end_offset)
{
  PangoLayoutIter *iter;
  PangoLayoutLine *line, *prev_line = NULL, *prev_prev_line = NULL;
  gint index, start_index, end_index;
  const gchar *text;
  gboolean found = FALSE;

  text = pango_layout_get_text (layout);
  index = g_utf8_offset_to_pointer (text, offset) - text;
  iter = pango_layout_get_iter (layout);
  do
    {
      line = pango_layout_iter_get_line (iter);
      start_index = line->start_index;
      end_index = start_index + line->length;

      if (index >= start_index && index <= end_index)
        {
          /* Found line for offset */
          if (prev_line)
            {
              switch (boundary_type)
                {
                default:
                  g_assert_not_reached();
                }
            }
          else
            start_index = end_index = 0;

          found = TRUE;
          break;
        }

      prev_prev_line = prev_line;
      prev_line = line;
    }
  while (pango_layout_iter_next_line (iter));

  if (!found)
    {
      start_index = prev_line->start_index + prev_line->length;
      end_index = start_index;
    }
  pango_layout_iter_free (iter);

  *start_offset = g_utf8_pointer_to_offset (text, text + start_index);
  *end_offset = g_utf8_pointer_to_offset (text, text + end_index);
}

static void
pango_layout_get_line_at (PangoLayout     *layout,
                          AtkTextBoundary  boundary_type,
                          gint             offset,
                          gint            *start_offset,
                          gint            *end_offset)
{
  PangoLayoutIter *iter;
  PangoLayoutLine *line, *prev_line = NULL;
  gint index, start_index, end_index;
  const gchar *text;
  gboolean found = FALSE;

  text = pango_layout_get_text (layout);
  index = g_utf8_offset_to_pointer (text, offset) - text;
  iter = pango_layout_get_iter (layout);
  do
    {
      line = pango_layout_iter_get_line (iter);
      start_index = line->start_index;
      end_index = start_index + line->length;

      if (index >= start_index && index <= end_index)
        {
          /* Found line for offset */
          switch (boundary_type)
            {
            default:
              g_assert_not_reached();
            }

          found = TRUE;
          break;
        }

      prev_line = line;
    }
  while (pango_layout_iter_next_line (iter));

  if (!found)
    {
      start_index = prev_line->start_index + prev_line->length;
      end_index = start_index;
    }
  pango_layout_iter_free (iter);

  *start_offset = g_utf8_pointer_to_offset (text, text + start_index);
  *end_offset = g_utf8_pointer_to_offset (text, text + end_index);
}

static void
pango_layout_get_line_after (PangoLayout     *layout,
                             AtkTextBoundary  boundary_type,
                             gint             offset,
                             gint            *start_offset,
                             gint            *end_offset)
{
  PangoLayoutIter *iter;
  PangoLayoutLine *line, *prev_line = NULL;
  gint index, start_index, end_index;
  const gchar *text;
  gboolean found = FALSE;

  text = pango_layout_get_text (layout);
  index = g_utf8_offset_to_pointer (text, offset) - text;
  iter = pango_layout_get_iter (layout);
  do
    {
      line = pango_layout_iter_get_line (iter);
      start_index = line->start_index;
      end_index = start_index + line->length;

      if (index >= start_index && index <= end_index)
        {
          /* Found line for offset */
          if (pango_layout_iter_next_line (iter))
            {
              line = pango_layout_iter_get_line (iter);
              switch (boundary_type)
                {
                default:
                  g_assert_not_reached();
                }
            }
          else
            start_index = end_index;

          found = TRUE;
          break;
        }

      prev_line = line;
    }
  while (pango_layout_iter_next_line (iter));

  if (!found)
    {
      start_index = prev_line->start_index + prev_line->length;
      end_index = start_index;
    }
  pango_layout_iter_free (iter);

  *start_offset = g_utf8_pointer_to_offset (text, text + start_index);
  *end_offset = g_utf8_pointer_to_offset (text, text + end_index);
}

/*
 * _gtk_pango_get_text_before:
 * @layout: a #PangoLayout
 * @boundary_type: a #AtkTextBoundary
 * @offset: a character offset in @layout
 * @start_offset: return location for the start of the returned text
 * @end_offset: return location for the end of the return text
 *
 * Gets a slice of the text from @layout before @offset.
 *
 * The @boundary_type determines the size of the returned slice of
 * text. For the exact semantics of this function, see
 * atk_text_get_text_before_offset().
 *
 * Returns: a newly allocated string containing a slice of text
 *     from layout. Free with g_free().
 */
gchar *
_gtk_pango_get_text_before (PangoLayout     *layout,
                            AtkTextBoundary  boundary_type,
                            gint             offset,
                            gint            *start_offset,
                            gint            *end_offset)
{
  const gchar *text;
  gint start, end;
  const PangoLogAttr *attrs;
  gint n_attrs;

  text = pango_layout_get_text (layout);

  if (text[0] == 0)
    {
      *start_offset = 0;
      *end_offset = 0;
      return g_strdup ("");
    }

  attrs = pango_layout_get_log_attrs_readonly (layout, &n_attrs);

  start = offset;
  end = start;
  *start_offset = start;
  *end_offset = end;

  g_assert (start <= end);

  return g_utf8_substring (text, start, end);
}

/*
 * _gtk_pango_get_text_after:
 * @layout: a #PangoLayout
 * @boundary_type: a #AtkTextBoundary
 * @offset: a character offset in @layout
 * @start_offset: return location for the start of the returned text
 * @end_offset: return location for the end of the return text
 *
 * Gets a slice of the text from @layout after @offset.
 *
 * The @boundary_type determines the size of the returned slice of
 * text. For the exact semantics of this function, see
 * atk_text_get_text_after_offset().
 *
 * Returns: a newly allocated string containing a slice of text
 *     from layout. Free with g_free().
 */
gchar *
_gtk_pango_get_text_after (PangoLayout     *layout,
                           AtkTextBoundary  boundary_type,
                           gint             offset,
                           gint            *start_offset,
                           gint            *end_offset)
{
  const gchar *text;
  gint start, end;
  const PangoLogAttr *attrs;
  gint n_attrs;

  text = pango_layout_get_text (layout);

  if (text[0] == 0)
    {
      *start_offset = 0;
      *end_offset = 0;
      return g_strdup ("");
    }

  attrs = pango_layout_get_log_attrs_readonly (layout, &n_attrs);

  start = offset;
  end = start;
  *start_offset = start;
  *end_offset = end;

  g_assert (start <= end);

  return g_utf8_substring (text, start, end);
}

/*
 * _gtk_pango_get_text_at:
 * @layout: a #PangoLayout
 * @boundary_type: a #AtkTextBoundary
 * @offset: a character offset in @layout
 * @start_offset: return location for the start of the returned text
 * @end_offset: return location for the end of the return text
 *
 * Gets a slice of the text from @layout at @offset.
 *
 * The @boundary_type determines the size of the returned slice of
 * text. For the exact semantics of this function, see
 * atk_text_get_text_after_offset().
 *
 * Returns: a newly allocated string containing a slice of text
 *     from layout. Free with g_free().
 */
gchar *
_gtk_pango_get_text_at (PangoLayout     *layout,
                        AtkTextBoundary  boundary_type,
                        gint             offset,
                        gint            *start_offset,
                        gint            *end_offset)
{
  const gchar *text;
  gint start, end;
  const PangoLogAttr *attrs;
  gint n_attrs;

  text = pango_layout_get_text (layout);

  if (text[0] == 0)
    {
      *start_offset = 0;
      *end_offset = 0;
      return g_strdup ("");
    }

  attrs = pango_layout_get_log_attrs_readonly (layout, &n_attrs);

  start = offset;
  end = start;
  *start_offset = start;
  *end_offset = end;

  g_assert (start <= end);

  return g_utf8_substring (text, start, end);
}

static gboolean
attr_list_merge_filter (PangoAttribute *attribute,
                        gpointer        list)
{
  pango_attr_list_change (list, pango_attribute_copy (attribute));
  return FALSE;
}

/*
 * _gtk_pango_attr_list_merge:
 * @into: a #PangoAttrList where attributes are merged or %NULL
 * @from: a #PangoAttrList with the attributes to merge or %NULL
 *
 * Merges attributes from @from into @into.
 *
 * Returns: the merged list.
 */
PangoAttrList *
_gtk_pango_attr_list_merge (PangoAttrList *into,
                            PangoAttrList *from)
{
  if (from)
    {
      if (into)
        pango_attr_list_filter (from, attr_list_merge_filter, into);
      else
       return pango_attr_list_ref (from);
    }

  return into;
}

PangoDirection
_gtk_pango_unichar_direction (gunichar ch)
{
  FriBidiCharType fribidi_ch_type;

  G_STATIC_ASSERT (sizeof (FriBidiChar) == sizeof (gunichar));

  fribidi_ch_type = fribidi_get_bidi_type (ch);

  if (!FRIBIDI_IS_STRONG (fribidi_ch_type))
    return PANGO_DIRECTION_NEUTRAL;
  else if (FRIBIDI_IS_RTL (fribidi_ch_type))
    return PANGO_DIRECTION_RTL;
  else
    return PANGO_DIRECTION_LTR;
}

PangoDirection
_gtk_pango_find_base_dir (const gchar *text,
                          gint         length)
{
  PangoDirection dir = PANGO_DIRECTION_NEUTRAL;
  const gchar *p;

  g_return_val_if_fail (text != NULL || length == 0, PANGO_DIRECTION_NEUTRAL);

  p = text;
  while ((length < 0 || p < text + length) && *p)
    {
      gunichar wc = g_utf8_get_char (p);

      dir = _gtk_pango_unichar_direction (wc);

      if (dir != PANGO_DIRECTION_NEUTRAL)
        break;

      p = g_utf8_next_char (p);
    }

  return dir;
}

