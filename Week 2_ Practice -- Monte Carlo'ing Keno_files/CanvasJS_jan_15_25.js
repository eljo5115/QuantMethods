// Google Tag Manager Code
(function(w,d,s,l,i){w[l]=w[l]||[];w[l].push({'gtm.start':
new Date().getTime(),event:'gtm.js'});var f=d.getElementsByTagName(s)[0],
j=d.createElement(s),dl=l!='dataLayer'?'&l='+l:'';j.async=true;j.src=
'https://www.googletagmanager.com/gtm.js?id='+i+dl;f.parentNode.insertBefore(j,f);
})(window,document,'script','dataLayer','GTM-MJ6HRCH');

var googleTagManager = '<noscript><iframe src="https://www.googletagmanager.com/ns.html?id=GTM-MJ6HRCH" height="0" width="0" style="display:none;visibility:hidden"></iframe></noscript>';
$('body').prepend(googleTagManager);

////////////////////////////////////////////////////
// DESIGNPLUS CONFIG                            //
////////////////////////////////////////////////////
// Legacy
var DT_variables = {
     iframeID: '',
        // Path to the hosted USU Design Tools
        path: 'https://designtools.ciditools.com/',
        templateCourse: '20467',
        // OPTIONAL: Button will be hidden from view until launched using shortcut keys
        hideButton: true,
    	 // OPTIONAL: Limit by course format
	     limitByFormat: false, // Change to true to limit by format
	     // adjust the formats as needed. Format must be set for the course and in this array for tools to load
	     formatArray: [
            'online',
            'on-campus',
            'blended'
        ],
        // OPTIONAL: Limit tools loading by users role
        limitByRole: false, // set to true to limit to roles in the roleArray
        // adjust roles as needed
        roleArray: [
            'student',
            'teacher',
            'admin'
        ],
        // OPTIONAL: Limit tools to an array of Canvas user IDs
        limitByUser: false, // Change to true to limit by user
        // add users to array (Canvas user ID not SIS user ID)
        userArray: [
            '1234',
            '987654'
        ]   // Paste variables from existing code here
};

// New
DpPrimary = {
    lms: 'canvas',
    templateCourse: '105491',
    hideButton: true,
    hideLti: false,
    extendedCourse: '', // added in sub-account theme
    sharedCourse: '', // added from localStorage
    courseFormats: [],
    canvasRoles: [],
    canvasUsers: [],
    canvasCourseIds: [],
    plugins: [],
    excludedModules: [],
    includedModules: [],
    lang: 'en',
    defaultToLegacy: false,
    enableVersionSwitching: true,
    hideSwitching: true,
}

// merge with extended/shared customizations config
DpConfig = { ...DpPrimary, ...(window.DpConfig ?? {}) }

$(function () {
    const uriPrefix = (location.href.includes('.beta.')) ? 'beta.' : '';
    const toolsUri = (DpConfig.toolsUri) ? DpConfig.toolsUri : `https://${uriPrefix}designplus.ciditools.com/`;
    $.getScript(`${toolsUri}js/controller.js`);
});
////////////////////////////////////////////////////
// END DESIGNPLUS CONFIG                        //
////////////////////////////////////////////////////

//Gradebook Feedback

let bootstrapCss = '<link href="https://{LTI_DOMAIN}/feedback/bootstrap/bootstrap.css?v=2.1.2" rel="stylesheet" crossorigin="anonymous">';
const bootstrapJs = 'https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/js/bootstrap.bundle.min.js';

const canvasUrlSelector = new RegExp("^(?:https?:\\/\\/)?(?:[^@\\/\\n]+@)?(?:www\\.)?([^\\/\\n]+)");
const courseIdSelector = new RegExp("(?<=/courses/)[0-9]+");
const canvasUrl = canvasUrlSelector.exec(window.location.href)[0];
const courseId = courseIdSelector.exec(window.location.href)[0];

const authLtiName = "Canvas Gradebook Feedback";
let ltiDomain;

const gradebookFeedbackHTML = `
<button type="button" tabindex="0" class="Button Button--primary dropdown-toggle" id="gradebookFeedbackButton" data-bs-toggle="dropdown" data-bs-auto-close="outside" aria-haspopup="true" aria-expanded="false">
Gradebook Feedback
</button>
  <div class="dropdown-menu" aria-labelledby="gradebookFeedbackButton">
      <form class="px-3 py-2 fb-box-sizing" id="feedbackSubmissionForm">
          <div class="row">
              <div class="col">
                  <h6 class="small text-center" tabindex="0" >How was your experience using the Canvas Gradebook today?</h6>
              </div>
          </div>
          <div class="row">
              <div class="col">
                  <div class="dropdown-divider"></div>
              </div>
          </div>
          <fieldset class="row justify-content-center">
              <div class="col-auto">
                  <div class="form-check form-check-inline">
                      <input class="btn-check" type="radio" name="emojiRadio" id="veryNegativeRadio" value="1"  onclick="$('#feedbackTextArea').collapse('show')" autocomplete="off">
                      <label class="btn btn-outline-primary form-check-label" for="veryNegativeRadio">&#128542;</label>
                  </div>
                  <div class="form-check form-check-inline">
                      <input class="btn-check" type="radio" name="emojiRadio" id="negativeRadio" value="2"  onclick="$('#feedbackTextArea').collapse('show')" autocomplete="off">
                      <label class="btn btn-outline-primary form-check-label" for="negativeRadio">&#128577;</label>
                  </div>
                  <div class="form-check form-check-inline">
                      <input class="btn-check" type="radio" name="emojiRadio" id="neutralRadio" value="3" onclick="$('#feedbackTextArea').collapse('show')" autocomplete="off">
                      <label class="btn btn-outline-primary form-check-label" for="neutralRadio">&#128528;</label>
                  </div>
                  <div class="form-check form-check-inline">
                      <input class="btn-check" type="radio" name="emojiRadio" id="positiveRadio" value="4" onclick="$('#feedbackTextArea').collapse('show')" autocomplete="off">
                      <label class="btn btn-outline-primary form-check-label" for="positiveRadio">&#128578;</label>
                  </div>
                  <div class="form-check form-check-inline">
                      <input class="btn-check" type="radio" name="emojiRadio" id="veryPositiveRadio" value="5" onclick="$('#feedbackTextArea').collapse('show')" autocomplete="off">
                      <label class="btn btn-outline-primary form-check-label" for="veryPositiveRadio">&#128515;</label>
                  </div>                         
              </div>
          </fieldset>
          <div class="collapse" id="feedbackTextArea">
              <div class="row">
                  <div class="col">
                      <div class="dropdown-divider"></div>
                  </div>
              </div>
              <div class="row">
                  <div class="col">
                      <div class="form-group">
                        <label for="textFeedbackInput" class="text-center">Please tell us more!</label>
                        <textarea class="form-control mt-2 mb-2 fb-box-sizing" id="textFeedbackInput" maxlength="1024" rows="4"></textarea>                               
                      </div>
                  </div>
              </div>
              <div class="row">
                  <div class="col mt-1">
                      <h6 class="small text-center" tabindex="0" >For help with the Gradebook, please contact the IT Service Center at 303-735-4357 or oithelp@colorado.edu</h6>
                  </div>
              </div>                    
              <div class="row">
                  <div class="col">
                      <div class="dropdown-divider"></div>
                  </div>
              </div>
              <div class="row" id="submitButtonRow">
                  <div class="col mt-1 text-center">
                      <button type="button" onClick="submitFeedback()" class="Button Button--primary" id="gradebookFeedbackSubmit">Submit</button> 
                  </div>
              </div>                                             
          </div>                                                                        
      </form>
  </div>`

const gradebookFeedbackCss = `
<style>
    #feedbackSubmissionForm {
        max-width: 300px;
    }

    .text-center {
        white-space: normal;
    }

    .form-check-inline {
        padding-left: 0rem;
        margin-right: 0rem;
    }

    .btn-outline-primary {
        border-color: transparent;
    }
</style>
`

function addFacultyGradebookButton() {
    $('head').append(gradebookFeedbackCss);
    $("#gradebook-actions").addClass('dropdown');
    $("#gradebook-actions").prepend(gradebookFeedbackHTML);
}

function addStudentGradebookButton() {
    waitForElement('#apply_select_menus').then((element) => {
        $('head').append(gradebookFeedbackCss);
        $('#apply_select_menus').parent().parent().append(gradebookFeedbackHTML);
    });
}

function showFeedbackError() {
    const errorHTML = `
            <div class="col">
                <p class="text-center text-danger fw-bold">
                    There was an error submitting your feedback. Please refresh the page and try again. If you continue to encounter issues, please contact the IT Service Center at 303-735-4357 or oithelp@colorado.edu.
                </p>
            </div>
    `

    let submitButtonRow = document.getElementById('submitButtonRow');
    submitButtonRow.innerHTML = errorHTML;
}

function showFeedbackSuccess() {
    const successHTML = `
        <div class="row">
            <div class="col">
                <p class="text-center fw-bold" >Thank you! Your feedback will help us improve the gradebook for CU students and educators.</p>
            </div>
        </div>
    `

    let feedbackForm = document.getElementById('feedbackSubmissionForm');
    feedbackForm.innerHTML = successHTML;
}

async function getLtiDomain() {
    try {
        const canvasApiResponse = await fetch(canvasUrl + "/api/v1/courses/" + courseId + "/external_tools?include_parents=true");
        if (!canvasApiResponse.ok) {
                throw new Error();
        } else {
            const canvasLtiApps = await canvasApiResponse.json();
            for (const ltiApp of canvasLtiApps) {
                if (ltiApp['name'] == authLtiName) {
                    return ltiApp['domain'];
                }
            }
        }
    } catch (e) {
        throw new Error("There was an error while communicating with the Canvas API while grabbing LTI domain");
    }

    throw new Error("Gradebook Feedback LTI Domain not found");
}

async function getLtiId() {
    try {
        const canvasApiResponse = await fetch(canvasUrl + "/api/v1/courses/" + courseId + "/external_tools?include_parents=true");
        if (!canvasApiResponse.ok) {
                throw new Error();
        } else {
            const canvasLtiApps = await canvasApiResponse.json();
            for (const ltiApp of canvasLtiApps) {
                if (ltiApp['name'] == authLtiName) {
                    return ltiApp['id'];
                }
            }
        }
    } catch (e) {
        throw new Error("There was an error while communicating with the Canvas API while grabbing the LTI ID");
    }

    throw new Error("Gradebook Feedback Auth LTI ID Not Found");
}

async function getLtiAuthData(authLtiId) {
    return new Promise((resolve, reject) => {
        let ltiFormData = new FormData();

        $.ajax({
            url: canvasUrl + "/courses/" + courseId + "/external_tools/" + authLtiId,
            method: "GET",
            success: function(html) {
                $(html).find("form :input").not(':button').each(function (){
                    ltiFormData.append(this.name, this.value)
                })
                resolve(ltiFormData);
            },
            error: function(html) {
                reject();
            }
        })
    });
}

async function getLtiToken() {
    let ltiId = await getLtiId();
    let ltiAuthData = await getLtiAuthData(ltiId);

    try {
        const ltiTokenResponse = await fetch (`https://${ltiDomain}/feedback/gradebook_feedback_lti`, {
            headers: {
                "Content-Type": "application/x-www-form-urlencoded",
            },
            method: "POST",
            body: new URLSearchParams(ltiAuthData)
        });
        if (!ltiTokenResponse.ok) {
                throw new Error();
        } else {
            const tokenResponseJson = await ltiTokenResponse.json();
            return tokenResponseJson['ltiToken'];
        }
    } catch (e) {
        throw new Error("There was an error while fetching the LTI Token");
    }
}

function waitForElement(selector) {
    return new Promise(resolve => {
        if (document.querySelector(selector)) {
            return resolve(document.querySelector(selector));
        }

        const observer = new MutationObserver(mutations => {
            if (document.querySelector(selector)) {
                observer.disconnect();
                resolve(document.querySelector(selector));
            }
        });

        observer.observe(document.body, {
            childList: true,
            subtree: true
        });
    });
}

async function submitFeedback() {
    let submitButton = document.getElementById('gradebookFeedbackSubmit');
    submitButton.setAttribute('disabled', true);
    submitButton.innerHTML = "Please Wait";

    let ltiToken;
    try {
        ltiToken = await getLtiToken();
    } catch (e) {
        showFeedbackError();
        throw e;
    }

    const feedbackData = {
        feedbackScore: document.querySelector('input[name="emojiRadio"]:checked').value,
        feedbackComments: document.getElementById('textFeedbackInput').value
    }

    try {
        const ltiTokenResponse = await fetch (`https://${ltiDomain}/feedback/rest/feedback/submit?lti_token=${encodeURIComponent(ltiToken)}`, {
            headers: {
                'Accept': 'application/json',
                'Content-Type': 'application/json'
            },
            method: "POST",
            body: JSON.stringify(feedbackData)
        });

        if (!ltiTokenResponse.ok) {
            throw new Error();
        }

        showFeedbackSuccess();

    } catch (e) {
        showFeedbackError();
        throw new Error("There was an error while submitting feedback");
    }
}

$(document).ready(async function () {
    'use strict';
    const currentUrl = window.location.href;

    if (currentUrl.includes("gradebook")) {
        ltiDomain = await getLtiDomain()
        $('head').append(bootstrapCss.replace("{LTI_DOMAIN}", ltiDomain))

        $.getScript(bootstrapJs, function () {
            addFacultyGradebookButton();
        });
    } else if (currentUrl.includes("grades")) {
        ltiDomain = await getLtiDomain()
        $('head').append(bootstrapCss.replace("{LTI_DOMAIN}", ltiDomain))

        $.getScript(bootstrapJs, function () {
            addStudentGradebookButton();
        });
    }
});