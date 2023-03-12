(in-package :cl-fzy)

(defun tty-init (tty tty-filename)
  (setf (tty-fdin tty) (open tty-filename :direction :input))
  (setf (tty-fout tty) (open tty-filename :direction :output :if-exists :append :if-does-not-exist :create))
  (setvbuf (tty-fout tty) nil :full-buffer 4096)
  (let ((original-termios (make-termios)))
    (sb-posix:tcgetattr (tty-fdin tty) original-termios)
    (let ((new-termios (copy-termios original-termios)))
      ;; Disable all of
      ;; ICANON  Canonical input (erase and kill processing).
      ;; ECHO    Echo.
      ;; ISIG    Signals from control characters
      ;; ICRNL   Conversion of CR characters into NL
      (setf (termios-icanon new-termios) nil
            (termios-echo new-termios) nil
            (termios-isig new-termios) nil
            (termios-icrnl new-termios) nil)
      (sb-posix:tcsetattr (tty-fdin tty) :now new-termios)
      (tty-getwinsz tty)))
  (tty-setnormal tty)
  (sb-posix:signal sb-posix:sigwinch #'handle-sigwinch))

(defun tty-getwinsz (tty)
  (let ((ws (make-winsize)))
    (if (sb-posix:ioctl (sb-unix:unix-fd-fileno (tty-fout tty)) sb-posix:tiocgwinsz ws)
        (setf (tty-maxwidth tty) (winsize-ws-col ws)
              (tty-maxheight tty) (winsize-ws-row ws))
        (setf (tty-maxwidth tty) 80
              (tty-maxheight tty) 25))))

(defun tty-reset (tty)
  (sb-posix:tcsetattr (tty-fdin tty) :now (tty-original-termios tty)))

(defun tty-close (tty)
  (tty-reset tty)
  (sb-posix:close (tty-fdin tty))
  (sb-posix:close (tty-fout tty)))


(defun tty-getchar (tty)
  (let ((ch) (size))
    (setq size (read (fd-stream tty) ch 0 1))
    (cond
      ((< size 0)
       (error "Error reading from tty"))
      ((= size 0)
       (exit :code 1))
      (t ch))))

(defun tty-input-ready (tty timeout return-on-signal)
  (let ((readfs (make-fdset tty)))
    (let ((ts (make-timespec
               :tv-sec (/ timeout 1000)
               :tv-nsec (* (mod timeout 1000) 1000000))))
      (let ((mask (make-sigset)))
        (if (not return-on-signal)
            (progn
              (sigaddset mask :sigwinch)))
        (let ((err (pselect tty (fdset-size readfs) readfs nil
                          nil (if return-on-signal nil mask) ts)))
          (if (< err 0)
              (if (= errno :eintr) 0
                  (error "select"))
              (fdset-member tty readfs)))))))

(defun tty-setwrap (tty)
  (tty-printf tty "%c%c?7h" #\Escape #\Open-Bracket))

(defun tty-newline (tty)
  (tty-printf tty "%c%cK\n" #\Escape #\Open-Bracket))

(defun tty-clearline (tty)
  (tty-printf tty "%c%cK" #\Escape #\Open-Bracket))

(defun tty-setcol (tty col)
  (tty-printf tty "%c%c%iG" #\Escape #\Open-Bracket (1+ col)))

(defun tty-moveup (tty i)
  (tty-printf tty "%c%c%iA" #\Escape #\Open-Bracket i))

(defun tty-printf (tty fmt &rest args)
  (apply #'format tty fmt args))

(defun tty-putc (tty c)
  (write-char c (tty-fout tty)))

(defun tty-flush (tty)
  (force-output (tty-fout tty)))

(defun tty-getwidth (tty)
  (tty-maxwidth tty))

(defun tty-getheight (tty)
  (tty-maxheight tty))


(defun tty-reset (tty)
  (tcsetattr (tty-fdin tty) tcsanow (tty-original-termios tty)))

(defun tty-close (tty)
  (tty-reset tty)
  (fclose (tty-fout tty))
  (close (tty-fdin tty)))

(defun handle-sigwinch (sig)
  (declare (ignore sig)))
