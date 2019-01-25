#ifndef _QDPIO_H
#define _QDPIO_H

#include <qio.h>
#include <qioxml.h>
#include <qdp_string.h>

/* volume formats */
#define QDP_SINGLEFILE QIO_SINGLEFILE 
#define QDP_MULTIFILE  QIO_MULTIFILE  

#ifdef __cplusplus
extern "C" {
#endif

  typedef struct QDP_Reader_struct QDP_Reader;
  typedef struct QDP_Writer_struct QDP_Writer;

  QDP_Writer *QDP_open_write(QDP_String *md, char *filename, int volfmt);
  QDP_Writer *QDP_open_write_L(QDP_Lattice *lat, QDP_String *md, char *filename, int volfmt);
  QDP_Writer *QDP_open_write_general_L(QDP_Lattice *lat, QDP_String *md, char *filename, int volfmt,
                                       QIO_Filesystem *fs_, QIO_Oflag *oflag_);
  int QDP_close_write(QDP_Writer *qw);

  QDP_Reader *QDP_open_read(QDP_String *md, char *filename);
  QDP_Reader *QDP_open_read_L(QDP_Lattice *lat, QDP_String *md, char *filename);
  QDP_Reader *QDP_open_read_general_L(QDP_Lattice *lat, QDP_String *md, char *filename, 
                                      QIO_Filesystem *fs_, QIO_Iflag *iflag_);
  int QDP_close_read(QDP_Reader *qr);

  QIO_Reader *QDP_reader_get_qio(QDP_Reader *qdpr);
  QIO_Writer *QDP_writer_get_qio(QDP_Writer *qdpw);

  int QDP_read_record_info(QDP_Reader *qr, QDP_String *md);
  int QDP_read_qio_record_info(QDP_Reader *qr, QIO_RecordInfo *ri, QDP_String *md);
  int QDP_next_record(QDP_Reader *qr);

  int QDP_set_read_group_size(int nodes);
  int QDP_set_write_group_size(int nodes);

  /* Read and write single field */

!ALLTYPES
  int QDP$PC_read_$ABBR(QDP_Reader *qr, QDP_String *md, $QDPPCTYPE *field);
  int QDP$PC_write_$ABBR(QDP_Writer *qw, QDP_String *md, $QDPPCTYPE *field);
!END

/* Read and write arrays of fields */

!ALLTYPES
  int QDP$PC_vread_$ABBR(QDP_Reader *qr, QDP_String *md,
			 $QDPPCTYPE *field[], int n);
  int QDP$PC_vwrite_$ABBR(QDP_Writer *qw, QDP_String *md,
			  $QDPPCTYPE *field[], int n);
!END

/* Read and write global array */

!ALLTYPES
  int QDP$PC_vread_$QLAABBR($NC QDP_Reader *qr, QDP_String *md,
			    $QLAPCTYPE($NCVAR(*field)), int n);
  int QDP$PC_vwrite_$QLAABBR($NC QDP_Writer *qw, QDP_String *md,
			     $QLAPCTYPE($NCVAR(*field)), int n);
!END

#ifdef __cplusplus
}
#endif

#endif /* _QDPIO_H */
