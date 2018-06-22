var child_process = require('child_process');

function getStdout(cmd) {
  var stdout = child_process.execSync(cmd);
  return stdout.toString().trim();
}

exports.host = "imap.gmail.com";
exports.port = 993;
exports.tls = true;
exports.username = "anand.padmanabha.iyer@gmail.com";
exports.password = getStdout("security find-generic-password -s emacs-email -a anand.padmanabha.iyer@gmail.com -w")
exports.onNotify = "afew -m; mbsync -q anand.padmanabha.iyer@gmail.com";
exports.onNotifyPost = {"mail": "terminal-notifier -message 'has new mail.' -title 'anand.padmanabha.iyer@gmail.com'; notmuch new; emacsclient -e '(mu4e-update-index)'"};
exports.boxes = ["INBOX"];
