exports._awaitSignIn = function () {
  return function (onError, onSuccess) {
    window.onSignIn = function (googleUser) {
       onSuccess(googleUser.getAuthResponse().id_token);
    };
    return function(cancelError, cancellerError, cancellerSuccess) {
      cancelError("Can't cancel");
    };
  };
};
