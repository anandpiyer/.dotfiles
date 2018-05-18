var child_process = require('child_process');

function getStdout(cmd) {
  var stdout = child_process.execSync(cmd);
  return stdout.toString().trim();
}

exports.host = "imap.gmail.com";
exports.port = 993;
exports.tls = true;
exports.username = "anand.iyer@berkeley.edu";
exports.password = getStdout("security find-generic-password -s emacs-email -a anand.iyer@berkeley.edu -w")
exports.onNotify = "mbsync -q bmail";
exports.onNotifyPost = {"mail": "terminal-notifier -message 'has new mail.' -title 'berkeley.edu'; emacsclient -e '(mu4e-update-index)'"};
exports.boxes = ["INBOX"];
