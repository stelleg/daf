/* ============================================================================
 * Freetype GL - A C OpenGL Freetype engine
 * Platform:    Any
 * WWW:         http://code.google.com/p/freetype-gl/
 * ----------------------------------------------------------------------------
 * Copyright 2011,2012 Nicolas P. Rougier. All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimer.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY NICOLAS P. ROUGIER ''AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
 * EVENT SHALL NICOLAS P. ROUGIER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * The views and conclusions contained in the software and documentation are
 * those of the authors and should not be interpreted as representing official
 * policies, either expressed or implied, of Nicolas P. Rougier.
 * ============================================================================
 */
#include "opengl.h"
#include "vec234.h"
#include "vector.h"

#include <stdlib.h>
#if defined(__APPLE__)
    #include <Glut/glut.h>
#elif defined(_WIN32) || defined(_WIN64)
     #include <GLUT/glut.h>
#else
    #include <GL/glut.h>
#endif
#include <stdio.h>
#include <wchar.h>
#include "arial-16.h"

void print_at( float scale, float pen_x, float pen_y, wchar_t *text )
{
    GLuint texid;
    glGenTextures( 1, &texid );
    glBindTexture( GL_TEXTURE_2D, texid );
    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP );
    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP );
    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR );
    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR );
    glTexImage2D( GL_TEXTURE_2D, 0, GL_ALPHA, font.tex_width, font.tex_height,
                  0, GL_ALPHA, GL_UNSIGNED_BYTE, font.tex_data );
    glBindTexture( GL_TEXTURE_2D, texid );
    glClear( GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT );

    glEnable( GL_BLEND );
    glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );
    glEnable( GL_TEXTURE_2D );
    size_t i, j;
    for( i=0; i<wcslen(text); ++i)
    {
        texture_glyph_t *glyph = 0;
        for( j=0; j<font.glyphs_count; ++j)
        {
            if( font.glyphs[j].charcode == text[i] )
            {
                glyph = &font.glyphs[j];
                break;
            }
        }
        if( !glyph )
        {
            continue;
        }
        float x = (pen_x + glyph->offset_x) / (1920 / scale);
        float y = (pen_y + glyph->offset_y) / (1080 / scale);
        float w  = glyph->width / (1920.0 / scale);
        float h  = glyph->height / (1080.0 / scale);
        glBegin( GL_TRIANGLES );
        {
            glTexCoord2f( glyph->s0, glyph->t0 ); glVertex2f( x,   y   );
            glTexCoord2f( glyph->s0, glyph->t1 ); glVertex2f( x,   y-h );
            glTexCoord2f( glyph->s1, glyph->t1 ); glVertex2f( x+w, y-h );
            glTexCoord2f( glyph->s0, glyph->t0 ); glVertex2f( x,   y   );
            glTexCoord2f( glyph->s1, glyph->t1 ); glVertex2f( x+w, y-h );
            glTexCoord2f( glyph->s1, glyph->t0 ); glVertex2f( x+w, y   );
        }
        glEnd();
        pen_x += glyph->advance_x;
        pen_y += glyph->advance_y;

    }
    glutSwapBuffers( );
}

