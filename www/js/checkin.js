(function () {
  // Clés publiques : sans risque à exposer côté client (protégées par RLS +
  // la fonction validate_attendance(), cf. supabase/schema.sql).
  var SUPABASE_URL = "https://ydsyylsnqtcxfakidmkq.supabase.co";
  var SUPABASE_PUBLISHABLE_KEY = "sb_publishable_1cUtK6bkW5t1OlyzIoh-BQ_i8GQVVvo";
  var FILL_SECONDS = 50; // doit correspondre à globals.R ; UX seulement, le serveur fait foi

  var sb = supabase.createClient(SUPABASE_URL, SUPABASE_PUBLISHABLE_KEY);

  var params = new URLSearchParams(window.location.search);
  var sessionId = params.get("s");
  var token = params.get("t");
  var course = params.get("course") || "";
  var date = params.get("date") || "";

  document.getElementById("session-info").textContent =
    course && date ? (course + " — " + date) : (course || "Unknown session");

  var geo = { lat: null, lon: null };
  if (navigator.geolocation) {
    navigator.geolocation.getCurrentPosition(
      function (pos) {
        geo.lat = pos.coords.latitude;
        geo.lon = pos.coords.longitude;
      },
      function () { /* denied/unavailable: le serveur tranchera si la géoloc est requise */ },
      { enableHighAccuracy: true, timeout: 15000 }
    );
  }

  var fingerprint = "";
  if (window.FingerprintJS) {
    FingerprintJS.load()
      .then(function (fp) { return fp.get(); })
      .then(function (result) { fingerprint = result.visitorId; })
      .catch(function () {});
  }

  var cdEl = document.getElementById("cd");
  var submitBtn = document.getElementById("submit");
  var startTime = Date.now();
  var timer = setInterval(function () {
    var remaining = Math.max(0, FILL_SECONDS - Math.floor((Date.now() - startTime) / 1000));
    cdEl.textContent = remaining + " s";
    if (remaining <= 0) {
      clearInterval(timer);
      submitBtn.disabled = true;
      showError("⏳ Time's up! The form has expired.");
    }
  }, 1000);

  function showError(msg) {
    document.getElementById("error_msg").textContent = msg || "";
    document.getElementById("success_msg").textContent = "";
  }
  function showSuccess(msg) {
    document.getElementById("success_msg").textContent = msg || "";
    document.getElementById("error_msg").textContent = "";
  }

  var REASON_MESSAGES = {
    SESSION_NOT_FOUND: "Unknown session. Please rescan the QR code.",
    SESSION_CLOSED: "This session is closed.",
    INVALID_TOKEN: "Invalid token. Please rescan the QR code.",
    TOKEN_EXPIRED: "Token expired. Please scan the QR code again.",
    LOCATION_REQUIRED: "📍 Location access is required to validate attendance.",
    DEVICE_ALREADY_USED: "This device has already been used to register for this session."
  };

  function messageFor(data) {
    if (data.reason === "OUT_OF_RANGE") {
      var dist = data.distance_m != null ? " (" + data.distance_m + " m)" : "";
      return "📍 You appear to be too far from the classroom" + dist + ".";
    }
    return REASON_MESSAGES[data.reason] || data.reason || "Unable to record attendance.";
  }

  submitBtn.addEventListener("click", async function () {
    showError("");

    if (!sessionId || !token) {
      showError("Invalid check-in link.");
      return;
    }

    var email = document.getElementById("email").value.trim().toLowerCase();
    var sid = document.getElementById("sid").value.trim();
    var lnid = document.getElementById("lnid").value.trim();
    var fnid = document.getElementById("fnid").value.trim();
    var master = document.getElementById("master").value;

    if (!email) return showError("Please enter your email.");
    if (!sid) return showError("Please enter your student Id.");
    if (!lnid) return showError("Please enter your last name.");
    if (!fnid) return showError("Please enter your first name.");

    submitBtn.disabled = true;
    submitBtn.textContent = "⏳ Submitting...";

    var result = await sb.rpc("validate_attendance", {
      p_session_id: sessionId,
      p_token: token,
      p_student_email: email,
      p_student_id: sid,
      p_master: master,
      p_first_name: fnid,
      p_last_name: lnid,
      p_fingerprint: fingerprint || null,
      p_lat: geo.lat,
      p_lon: geo.lon
    });

    if (result.error) {
      showError("Error recording attendance. Please retry.");
      submitBtn.disabled = false;
      submitBtn.textContent = "I'm present";
      return;
    }

    if (!result.data.ok) {
      showError(messageFor(result.data));
      submitBtn.disabled = false;
      submitBtn.textContent = "I'm present";
      return;
    }

    clearInterval(timer);
    document.getElementById("form_zone").style.display = "none";
    showSuccess("✓ Attendance recorded for " + email + ". You can close this window.");
  });
})();
